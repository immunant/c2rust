//
//  FloatingLexer.cpp
//  LLVMDemangle
//
//  Created by Eric Mertens on 11/5/18.
//

#include "FloatingLexer.h"

using std::string;

// Extract a floating point literal from a given character buffer.
// This function will elaborate the literal to something Rust can handle
//
// This function assumes that it's only lexing valid lexeme prefixes
// because it's being run after the clang lexer.
//
// "1." -> "1.0"
// ".1" -> "0.1"
// "1.000" -> "1.000"
// "1.2e+3" -> "1.2e+3"
// "1e-5" -> "1e-5"
string matchFloatingLiteral(const char * prefix) {
    
    // Hex literals are not supported
    if (strcmp("0x", prefix) == 0 || strcmp("0X", prefix) == 0) {
        return "";
    }
    
    string output;
    
    // detect when there are no digits before the decimal
    bool hasDigits = false;
    for (;;) {
        auto c = *prefix++;
        if ('0' <= c && c <= '9') {
            output.push_back(c);
            hasDigits = true;
        } else if ('.' == c) {
            if (!hasDigits) {
                output.push_back('0');
            }
            output.push_back(c);
            goto fracpart;
        } else if ('E' == c || 'e' == c) {
            output.push_back(c);
            goto exppart;
        } else {
            // something's wrong, treat this as unlexable
            return "";
        }
    }
    
fracpart:
    // detect when we have a decimal but no digits, Rust doesn't allow this
    hasDigits = false;
    for (;;) {
        auto c = *prefix++;
        
        if ('0' <= c && c <= '9') { // decimal digit
            output.push_back(c);
            hasDigits = true;
        } else if('E' == c || 'e' == c) { // start of exponent
            if (!hasDigits) {
                output.push_back('0');
            }
            output.push_back(c);
            goto exppart;
        } else { // end of literal
            if (!hasDigits) {
                output.push_back('0');
            }
            return output;
        }
    }

exppart:
    // detect if we're at the beginning of the exponent where a + or - is allowed
    hasDigits = false;
    for (;;) {
        auto c = *prefix++;
        
        if ('0' <= c && c <= '9') {
            output.push_back(c);
            hasDigits = true;
        } else if (!hasDigits && (c == '+' || c == '-')) {
            output.push_back(c);
            hasDigits = true;
        } else {
            return output;
        }
    }
}
