#pragma once
#include <string>
#include "parser.h"
#include "codewriter.h"

class VMTranslator
{
public:
    VMTranslator();
    /**
     * Intitializes a parser and codeWriter object.
     */
    VMTranslator(std::string outputFileName);
    /**
     * Advances the parser until the EOF.
     * Separates Arithmetic, Pop and Push commands from each other.
     */
    void start();

private:
    Parser *parser;
    CodeWriter *codeWriter;
};