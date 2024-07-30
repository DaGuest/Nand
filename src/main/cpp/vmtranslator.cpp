#include "vmtranslator.h"

VMTranslator::VMTranslator() {}

VMTranslator::VMTranslator(std::string fileName)
{
    parser = new Parser(fileName);
}

void VMTranslator::start()
{
    while (parser->hasMoreLines())
    {
        parser->advance();
    }
}

int main(int argc, char const *argv[])
{
    VMTranslator vmtranslator("../../test/resources/Project 7/Test.vm");
    vmtranslator.start();
    return 0;
}
