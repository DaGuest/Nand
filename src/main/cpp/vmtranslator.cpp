#include "vmtranslator.h";

VMTranslator::VMTranslator(std::string fileName)
{
    parser = Parser(fileName);
}

void VMTranslator::start()
{
    while (parser.hasMoreLines())
    {
        parser.advance();
    }
}