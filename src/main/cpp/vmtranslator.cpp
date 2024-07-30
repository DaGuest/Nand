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
        if (parser->commandType() == Parser::C_PUSH)
        {
            std::cout << parser->arg2() << std::endl;
        }
    }
}

int main(int argc, char const *argv[])
{
    VMTranslator vmtranslator("../../test/resources/Project 7/BasicTest.vm");
    vmtranslator.start();
    return 0;
}
