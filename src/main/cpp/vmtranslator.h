#pragma once
#include <string>;
#include "parser.h";

class VMTranslator
{
public:
    VMTranslator(std::string fileName);
    void start();

private:
    Parser parser;
};