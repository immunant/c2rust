//
//  ExportResult.hpp
//  LLVMDemangle
//
//  Created by Eric Mertens on 9/11/18.
//

#ifndef ExportResult_hpp
#define ExportResult_hpp

#include <algorithm>
#include <cstddef>
#include <cstdint>

struct ExportResult {
    std::size_t entries;
    char **names;
    std::uint8_t **bytes;
    std::size_t *sizes;
    // Preprocessed source text per entry; entries may be null when
    // preprocessed output was not requested or could not be produced.
    char **preprocessed;

    ExportResult();
    ExportResult(ExportResult const &) = delete;
    ExportResult &operator=(ExportResult const &) = delete;

    ~ExportResult();

    void resize(std::size_t n);

  private:
    void deallocate();
};

#endif /* ExportResult_hpp */
