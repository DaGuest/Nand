#pragma once
#include <string>
#include "parser.h"
#include "codewriter.h"
#include <vector>
#include <filesystem>

class VMTranslator
{
public:
    VMTranslator();

    /**
     * Intitializes a parser and codeWriter object.
     */
    VMTranslator(std::string inputPath);

    /**
     * Checks for directory or filename.
     * Translates all .vm files given.
     */
    void start();

    /**
     * Translates a .vm file into assembly code
     */
    void translateFile(std::filesystem::path path);

private:
    Parser *parser;
    CodeWriter *codeWriter;
    std::filesystem::path path;
};