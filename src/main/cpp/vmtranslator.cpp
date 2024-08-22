#include "vmtranslator.h"

VMTranslator::VMTranslator() {}

VMTranslator::VMTranslator(std::string inputFileName, std::string outputFileName)
{
    parser = new Parser(inputFileName);
    codeWriter = new CodeWriter(outputFileName, inputFileName);
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
    VMTranslator vmtranslator("../../test/resources/Project 7/BasicTest.vm", "../../test/resources/Project 7/output.asm");
    vmtranslator.start();
    return 0;
}
