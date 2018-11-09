//
//  Main.cpp
//
//  Created by Alec Theriault on 10/4/18.
//
#include <fstream>
#include <iterator>

#include "AstExporter.hpp"

int main(int argc, char *argv[]) {
  int result;
  auto outputs = process(argc, const_cast<const char **>(argv), &result);

  for (auto const &kv : outputs) {
      auto const &filename = kv.first;
      auto const &bytes    = kv.second;

      std::ofstream out(filename + ".cbor", out.binary | out.trunc);

      out.write(reinterpret_cast<const char*>(bytes.data()), bytes.size());
  }

  return result;
}
