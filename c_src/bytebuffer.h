#ifndef EZLIB_C_SRC_BYTEBUFFER_H_
#define EZLIB_C_SRC_BYTEBUFFER_H_

#include "macros.h"

#include <stdlib.h>

class ByteBuffer
{

public:
    
    ByteBuffer(size_t size);
    ByteBuffer(const char* bytes, size_t len);
    explicit ByteBuffer(const char* bytes);
    
    ~ByteBuffer();
    
    const char* Data() const { return bytes_ + start_; }
    size_t Length() const { return end_ - start_; }
    size_t Capacity() const { return size_ - start_; }
    
    bool ReadBytes(char* val, size_t len);
    void WriteBytes(const char* val, size_t len);
    
    char* ReserveWriteBuffer(size_t len);
    void Resize(size_t size);
    bool Consume(size_t size);
    void Clear();
    
private:
    
    void Construct(const char* bytes, size_t size);
    
    char* bytes_;
    size_t size_;
    size_t start_;
    size_t end_;
    
    DISALLOW_COPY_AND_ASSIGN(ByteBuffer);
};

#endif
