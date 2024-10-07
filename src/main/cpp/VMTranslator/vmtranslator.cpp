#include "vmtranslator.h"

VMTranslator::VMTranslator() {}

VMTranslator::VMTranslator(std::string inputPath)
{
    path = std::filesystem::path(inputPath);
}

void VMTranslator::start()
{
    if (path.has_extension())
    {
        codeWriter = new CodeWriter(path.parent_path().string() + "/" + path.stem().string());
        translateFile(std::filesystem::path(path));
    }
    else
    {
        codeWriter = new CodeWriter(path.string() + "/" + path.filename().string());
        if (std::filesystem::exists(path.append("Sys.vm")))
        {
            codeWriter->setFileName(path.stem());
            codeWriter->writeBootStrap();
        }
        path.remove_filename();
        for (const auto &fileName : std::filesystem::directory_iterator(path))
        {
            if (fileName.path().extension() == ".vm")
            {
                translateFile(fileName.path());
            }
        }
    }
}

void VMTranslator::translateFile(std::filesystem::path path)
{
    parser = new Parser(path);
    codeWriter->setFileName(path.stem());

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
            break;
        case Parser::C_FUNCTION:
            codeWriter->writeFunction(parser->arg1(), parser->arg2());
            break;
        case Parser::C_RETURN:
            codeWriter->writeReturn();
            break;
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
