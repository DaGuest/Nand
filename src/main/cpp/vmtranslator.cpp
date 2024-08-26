#include "vmtranslator.h"

VMTranslator::VMTranslator() {}

VMTranslator::VMTranslator(std::string inputFileName)
{
    parser = new Parser(inputFileName);
    codeWriter = new CodeWriter(inputFileName);
}

void VMTranslator::start()
{
    while (parser->hasMoreLines())
    {
        parser->advance();
        if (parser->commandType() == Parser::C_ARITHMETIC)
        {
            codeWriter->writeArithmetic(parser->arg1());
        }
        else if (parser->commandType() == Parser::C_POP)
        {
            codeWriter->writePushPop(Parser::C_POP, parser->arg1(), parser->arg2());
        }
        else if (parser->commandType() == Parser::C_PUSH)
        {
            codeWriter->writePushPop(Parser::C_PUSH, parser->arg1(), parser->arg2());
        }
    }
}

int main(int argc, char const *argv[])
{
    VMTranslator vmtranslator(argv[1]);
    vmtranslator.start();
    return 0;
}
