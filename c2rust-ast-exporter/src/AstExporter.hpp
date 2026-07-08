//
//  AstExporter.hpp
//
//  Created by Alec Theriault on 10/4/18.
//

#ifndef AstExporter_hpp
#define AstExporter_hpp

#include <string>
#include <unordered_map>
#include <vector>

using Outputs = std::unordered_map<std::string, std::vector<uint8_t>>;
using PreprocessedOutputs = std::unordered_map<std::string, std::string>;

Outputs process(int argc, const char *argv[], int *result,
                PreprocessedOutputs *preprocessed = nullptr);

#endif /* AstExporter_hpp */
