#pragma once
#include <string>
#include "parser.h"
#include "codewriter.h"

class VMTranslator
{
public:
    VMTranslator();
    VMTranslator(std::string outputFileName);
    void start();

private:
    Parser *parser;
    CodeWriter *codeWriter;
};