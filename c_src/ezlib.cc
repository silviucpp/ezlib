#include "ezlib.h"
#include "bytebuffer.h"
#include "ezlib_nif.h"
#include "nif_utils.h"
#include "macros.h"
#include "zlib2/zlib.h"

#include <string.h>
#include <string>
#include <memory>

#define UINT64_METRIC(Name, Property) enif_make_tuple2(env, Name, enif_make_uint64(env, Property))
#define DOUBLE_METRIC(Name, Property) enif_make_tuple2(env, Name, enif_make_double(env, Property))

#define DEFLATE 1
#define INFLATE 2
#define CHUNK_SIZE 1024

#define DEFAULT_BUFFER_CAPACITY 1024
#define MAX_BUFFER_CAPACITY 8192

#define scoped_ptr(Name, Type, New, Free) std::unique_ptr<Type, decltype(&Free)>Name (New, &Free)

const char kErrorCreateStreamFailed[] = "create_stream failed";
const char kErrorDeflateInitFailed[] = "deflateInit failed";
const char kErrorInflateInitFailed[] = "inflateInit failed";
const char kErrorAllocResourceFailed[] = "enif_alloc_resource failed";
const char kErrorBadOwner[] = "ezlib session was created on a different process";

typedef int (*PROCESSING_FUNCTION)(z_stream* strm, int flush);

#if defined(USE_CUSTOM_ALLOCATOR)
voidpf z_alloc(voidpf opaque, uInt items, uInt size)
{
    UNUSED(opaque);
    return enif_alloc(items*size);
}

void z_free(voidpf opaque, voidpf address)
{
    UNUSED(opaque);
    return enif_free(address);
}
#endif

struct zlib_session
{
    ByteBuffer* buffer;
    z_stream*  stream;
    unsigned char method;
    PROCESSING_FUNCTION processing_function;
    bool use_iolist;
#if defined CHECK_CALLER_PROCESS
    ERL_NIF_TERM owner_pid;
#endif
};

struct zlib_options
{
    zlib_options() :
        compression_level(Z_DEFAULT_COMPRESSION),
        window_bits(15),
        mem_level(8),
        compression_strategy(Z_DEFAULT_STRATEGY),
        use_iolist(false) {}

    int compression_level;
    int window_bits;
    int mem_level;
    int compression_strategy;
    bool use_iolist;
};

z_stream* create_stream()
{
    z_stream* stream = static_cast<z_stream*>(enif_alloc(sizeof(z_stream)));
    
    if(!stream)
        return NULL;
    
    memset(stream, 0, sizeof(z_stream));
    stream->data_type = Z_BINARY;
#if defined(USE_CUSTOM_ALLOCATOR)
    stream->zalloc = z_alloc;
    stream->zfree = z_free;
#endif
    return stream;
}

void nif_zlib_session_free(ErlNifEnv* env, void* obj)
{
    UNUSED(env);
    
    zlib_session *session = static_cast<zlib_session*>(obj);
    
    if(session->stream)
    {
        if(session->method == DEFLATE)
            deflateEnd(session->stream);
        else
            inflateEnd(session->stream);
        
        enif_free(session->stream);
    }
    
    if(session->buffer)
        delete session->buffer;
}

bool process_buffer(zlib_session* session, unsigned char* data, size_t len)
{
    if(!len)
        return true;

    int result;
    size_t bytes_to_write;
    unsigned char chunk[CHUNK_SIZE];
    
    session->stream->avail_in = len;
    session->stream->next_in = data;
    
    do
    {
        session->stream->avail_out = CHUNK_SIZE;
        session->stream->next_out =  chunk;
        
        result = session->processing_function(session->stream, Z_SYNC_FLUSH);

        // Output buffer was completely consumed and we have no more data to process
        // http://www.zlib.net/zlib_faq.html#faq05
        if(session->method == INFLATE && result == Z_BUF_ERROR && session->stream->avail_out == CHUNK_SIZE)
            return true;

        if (result != Z_OK)
            return false;
        
        bytes_to_write = CHUNK_SIZE - session->stream->avail_out;
        
        if(bytes_to_write > 0)
            session->buffer->WriteBytes(reinterpret_cast<const char*>(chunk), bytes_to_write);
    }
    while (session->stream->avail_out == 0);
    
    return true;
}

ERL_NIF_TERM parse_options(ErlNifEnv* env, ERL_NIF_TERM options, zlib_options* out)
{
    if(!enif_is_list(env, options))
        return make_badarg(env);

    const ERL_NIF_TERM *items;
    int arity;

    ERL_NIF_TERM head;

    while(enif_get_list_cell(env, options, &head, &options))
    {
        if(!enif_get_tuple(env, head, &arity, &items) || arity != 2)
            return make_bad_options(env, head);

        ERL_NIF_TERM key = items[0];
        ERL_NIF_TERM value = items[1];

        if(enif_is_identical(key, ATOMS.atomCompressionLevel))
        {
            if(!enif_get_int(env, value, &out->compression_level))
                return make_bad_options(env, head);
        }
        else if(enif_is_identical(key, ATOMS.atomWindowBits))
        {
            if(!enif_get_int(env, value, &out->window_bits))
                return make_bad_options(env, head);
        }
        else if(enif_is_identical(key, ATOMS.atomMemLevel))
        {
            if(!enif_get_int(env, value, &out->mem_level))
                return make_bad_options(env, head);
        }
        else if(enif_is_identical(key, ATOMS.atomCompStrategy))
        {
            if(!enif_get_int(env, value, &out->compression_strategy))
                return make_bad_options(env, head);
        }
        else if(enif_is_identical(key, ATOMS.atomUseIoList))
        {
            if(!get_boolean(value, &out->use_iolist))
                return make_bad_options(env, head);
        }
        else
        {
            return make_bad_options(env, head);
        }
    }

    return ATOMS.atomOk;
}


ERL_NIF_TERM nif_zlib_new_session(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ezlib_data* data = static_cast<ezlib_data*>(enif_priv_data(env));
    
    int method;
    
    if(!enif_get_int(env, argv[0], &method))
        return make_badarg(env);
    
    if(method != DEFLATE && method != INFLATE)
        return make_badarg(env);

    zlib_options opt;
    
    if(argc == 2)
    {
        ERL_NIF_TERM options_result = parse_options(env, argv[1], &opt);

        if(!enif_is_identical(options_result, ATOMS.atomOk))
            return options_result;
    }
    
    scoped_ptr(stream, z_stream, create_stream(), enif_free);
    
    if(stream.get() == NULL)
        return make_error(env, kErrorCreateStreamFailed);
    
    if(method == DEFLATE)
    {
        if(deflateInit2(stream.get(), opt.compression_level, Z_DEFLATED, opt.window_bits, opt.mem_level, opt.compression_strategy) != Z_OK)
            return make_error(env, kErrorDeflateInitFailed);
    }
    else
    {
        if(inflateInit2(stream.get(), opt.window_bits) != Z_OK)
            return make_error(env, kErrorInflateInitFailed);
    }
    
    zlib_session* session = static_cast<zlib_session*>(enif_alloc_resource(data->resZlibSession, sizeof(zlib_session)));
    
    if(session == NULL)
    {
        if(method == DEFLATE)
            deflateEnd(stream.get());
        else
            inflateEnd(stream.get());
   
        return make_error(env, kErrorAllocResourceFailed);
    }
    
    memset(session, 0, sizeof(zlib_session));
    
    session->buffer = new ByteBuffer(DEFAULT_BUFFER_CAPACITY);
    session->method = method;
    session->stream = stream.release();
    session->processing_function = (method == DEFLATE ? deflate : inflate);
    session->use_iolist = opt.use_iolist;
#if defined CHECK_CALLER_PROCESS
    ErlNifPid current_pid;
    enif_self(env, &current_pid);
    session->owner_pid = enif_make_pid(env, &current_pid);
#endif
    
    ERL_NIF_TERM term = enif_make_resource(env, session);
    enif_release_resource(session);
    
    return enif_make_tuple2(env, ATOMS.atomOk, term);
}

ERL_NIF_TERM nif_zlib_process_buffer(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    
    ezlib_data* data = static_cast<ezlib_data*>(enif_priv_data(env));
    
    zlib_session* session = NULL;
    ErlNifBinary in_buffer;
    
    if(!enif_get_resource(env, argv[0], data->resZlibSession, (void**) &session))
        return make_badarg(env);
    
    if(!get_binary(env, argv[1], &in_buffer))
        return make_badarg(env);
    
#if defined CHECK_CALLER_PROCESS
    ErlNifPid current_pid;
    
    if(enif_self(env, &current_pid) && !enif_is_identical(session->owner_pid, enif_make_pid(env, &current_pid)))
        return make_error(env, kErrorBadOwner);
#endif
    
    if(!process_buffer(session, in_buffer.data, in_buffer.size))
    {
        std::string error("process_buffer failed: ");
        
        if(session->stream->msg)
            error.append(session->stream->msg);
        
        return make_error(env, error.c_str());
    }

    size_t length = session->buffer->Length();
    
    ERL_NIF_TERM return_term;
    
    if(session->use_iolist)
        return_term = enif_make_string_len(env, session->buffer->Data(), length, ERL_NIF_LATIN1);
    else
        return_term = make_binary(env, session->buffer->Data(), length);
    
    session->buffer->Consume(length);
    
    if(session->buffer->Capacity() > MAX_BUFFER_CAPACITY)
        session->buffer->Resize(DEFAULT_BUFFER_CAPACITY);
    
    return return_term;
}

ERL_NIF_TERM nif_get_stats(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    
    ezlib_data* data = static_cast<ezlib_data*>(enif_priv_data(env));
    
    zlib_session* session = NULL;
    
    if(!enif_get_resource(env, argv[0], data->resZlibSession, (void**) &session))
        return make_badarg(env);
    
    double ratio = 0;
    
    if(session->stream->total_in > 0 && session->stream->total_out > 0)
    {
        if(session->method == DEFLATE)
            ratio = (1.0f - (static_cast<double>(session->stream->total_out)/static_cast<double>(session->stream->total_in)))*100;
        else
            ratio = (1.0f - (static_cast<double>(session->stream->total_in)/static_cast<double>(session->stream->total_out)))*100;
    }
    
    ERL_NIF_TERM stats = enif_make_tuple(env, 3, UINT64_METRIC(ATOMS.atomBytesIn, session->stream->total_in),
                                                 UINT64_METRIC(ATOMS.atomBytesOut, session->stream->total_out),
                                                 DOUBLE_METRIC(ATOMS.atomCompressionRatio, ratio));
    
    return enif_make_tuple2(env, ATOMS.atomOk, stats);
}



