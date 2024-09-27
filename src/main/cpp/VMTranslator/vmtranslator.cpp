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
        switch (parser->commandType())
        {
        case Parser::C_ARITHMETIC:
            codeWriter->writeArithmetic(parser->arg1());
            break;
        case Parser::C_POP:
            codeWriter->writePushPop(Parser::C_POP, parser->arg1(), parser->arg2());
            break;
        case Parser::C_PUSH:
            codeWriter->writePushPop(Parser::C_PUSH, parser->arg1(), parser->arg2());
            break;
        case Parser::C_GOTO:
            codeWriter->writeGoto(parser->arg1());
            break;
        case Parser::C_LABEL:
            codeWriter->writeLabel(parser->arg1());
            break;
        case Parser::C_IF:
            codeWriter->writeIf(parser->arg1());
            break;
        case Parser::C_CALL:
            codeWriter->writeCall(parser->arg1(), parser->arg2());
        default:
            break;
        }
    }
}

int main(int argc, char const *argv[])
{
    VMTranslator vmtranslator(argv[1]);
    vmtranslator.start();
    return 0;
}
