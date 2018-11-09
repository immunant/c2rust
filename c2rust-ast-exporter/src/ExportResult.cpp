//
//  ExportResult.cpp
//  LLVMDemangle
//
//  Created by Eric Mertens on 9/11/18.
//

#include "ExportResult.hpp"

ExportResult::ExportResult()
  : entries(0), names(), bytes(), sizes() {}

ExportResult::~ExportResult() { deallocate(); }

void ExportResult::resize(std::size_t n) {
    deallocate();
    names = new char*[n];
    bytes = new std::uint8_t*[n];
    sizes = new std::size_t[n];
    std::fill_n(names, n, nullptr);
    std::fill_n(bytes, n, nullptr);
    std::fill_n(sizes, n, 0);
    entries = n;
}

void ExportResult::deallocate() {
    for (std::size_t i = 0; i < entries; i++) {
        delete [] names[i];
        delete [] bytes[i];
    }
    delete [] names;
    delete [] bytes;
    delete [] sizes;

    entries = 0;
}
