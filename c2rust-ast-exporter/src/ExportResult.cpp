//
//  ExportResult.cpp
//  LLVMDemangle
//
//  Created by Eric Mertens on 9/11/18.
//

#include "ExportResult.hpp"

ExportResult::ExportResult()
    : entries(0), names(), bytes(), sizes(), preprocessed() {}

ExportResult::~ExportResult() { deallocate(); }

void ExportResult::resize(std::size_t n) {
    deallocate();
    names = new char *[n];
    bytes = new std::uint8_t *[n];
    sizes = new std::size_t[n];
    preprocessed = new char *[n];
    std::fill_n(names, n, nullptr);
    std::fill_n(bytes, n, nullptr);
    std::fill_n(sizes, n, 0);
    std::fill_n(preprocessed, n, nullptr);
    entries = n;
}

void ExportResult::deallocate() {
    for (std::size_t i = 0; i < entries; i++) {
        delete[] names[i];
        delete[] bytes[i];
        delete[] preprocessed[i];
    }
    delete[] names;
    delete[] bytes;
    delete[] sizes;
    delete[] preprocessed;

    entries = 0;
}
