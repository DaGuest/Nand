#include "parser.h"

Parser::Parser() {};

Parser::Parser(std::string fileName)
{
    inputFile.open(fileName);
}

void Parser::close()
{
    inputFile.close();
}

bool Parser::hasMoreLines()
{
    return inputFile.peek() != EOF;
}

void Parser::advance()
{
    getline(inputFile, currentCommand);
    while (!checkLine())
    {
        if (!hasMoreLines())
        {
            return;
        }
        else
        {
            getline(inputFile, currentCommand);
        }
    }
    splitString(currentCommand);
}

Parser::CommandType Parser::commandType()
{
    // Check for push/pop command
    if (splitCommands.size() > 1)
    {
        if (splitCommands.front() == "push")
        {
            return Parser::C_PUSH;
        }
        else
        {
            return Parser::C_POP;
        }
    }
    // Else: arithmetic command
    else
    {
        return Parser::C_ARITHMETIC;
    }
}

std::string Parser::arg1()
{
    if (commandType() == Parser::C_ARITHMETIC)
    {
        return splitCommands.front();
    }
    else
    {
        return splitCommands.at(1);
    }
}

int Parser::arg2()
{
    return std::stoi(splitCommands.at(2));
}

void Parser::splitString(std::string stringToSplit)
{
    splitCommands.clear();
    size_t pos = 0;
    std::string command;
    while ((pos = stringToSplit.find(" ")) != std::string::npos)
    {
        command = stringToSplit.substr(0, pos);
        splitCommands.push_back(command);
        stringToSplit.erase(0, pos + 1);
    }
    splitCommands.push_back(stringToSplit);
}

bool Parser::checkLine()
{
    return !(currentCommand.front() == '/' || currentCommand.empty());
}