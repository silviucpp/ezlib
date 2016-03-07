#include "bytebuffer.h"
#include "nif.h"

#include <algorithm>
#include <string.h>

ByteBuffer::ByteBuffer(size_t size)
{
    Construct(NULL, size);
}

ByteBuffer::ByteBuffer(const char* bytes, size_t len)
{
    Construct(bytes, len);
}

ByteBuffer::ByteBuffer(const char* bytes)
{
    Construct(bytes, strlen(bytes));
}

void ByteBuffer::Construct(const char* bytes, size_t len)
{
    start_ = 0;
    size_ = len;
    bytes_ = static_cast<char*>(enif_alloc(size_));
    
    if (bytes)
    {
        end_ = len;
        memcpy(bytes_, bytes, end_);
    }
    else
    {
        end_ = 0;
    }
}

ByteBuffer::~ByteBuffer()
{
    enif_free(bytes_);
}

bool ByteBuffer::ReadBytes(char* val, size_t len)
{
    if (len > Length())
        return false;

    memcpy(val, bytes_ + start_, len);
    start_ += len;
    return true;
}

void ByteBuffer::WriteBytes(const char* val, size_t len)
{
    memcpy(ReserveWriteBuffer(len), val, len);
}

char* ByteBuffer::ReserveWriteBuffer(size_t len)
{
    if (Length() + len > Capacity())
        Resize(Length() + len);
    
    char* start = bytes_ + end_;
    end_ += len;
    return start;
}

void ByteBuffer::Resize(size_t size)
{
    size_t len = std::min(end_ - start_, size);
    if (size <= size_)
    {
        // Don't reallocate, just move data backwards
        memmove(bytes_, bytes_ + start_, len);
    }
    else
    {
        // Reallocate a larger buffer.
        size_ = std::max(size, 3 * size_ / 2);
        char* new_bytes = static_cast<char*>(enif_alloc(size_));
        memcpy(new_bytes, bytes_ + start_, len);

        enif_free(bytes_);
        bytes_ = new_bytes;
    }
    
    start_ = 0;
    end_ = len;
}

bool ByteBuffer::Consume(size_t size)
{
    if (size > Length())
        return false;

    start_ += size;
    return true;
}

void ByteBuffer::Clear()
{
    memset(bytes_, 0, size_);
    start_ = end_ = 0;
}
