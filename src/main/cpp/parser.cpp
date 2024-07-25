#include "parser.h";

Parser::Parser() {};

Parser::Parser(std::string fileName)
{
    inputFile.open(fileName);
}

void Parser::close()
{
}

bool Parser::hasMoreLines()
{
    return inputFile.peek() != EOF;
}

void Parser::advance()
{
    while (hasMoreLines())
    {
        getline(inputFile, currentCommand);
    }
}

Parser::CommandType Parser::commandType()
{
}