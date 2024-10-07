#include "parser.h"

Parser::Parser() {};

Parser::Parser(std::string inputPath)
{
    inputFile.open(inputPath);
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
    currentCommand = stripFront(currentCommand);
    while (!checkLine())
    {
        if (!hasMoreLines())
        {
            return;
        }
        else
        {
            getline(inputFile, currentCommand);
            currentCommand = stripFront(currentCommand);
        }
    }
    splitString(currentCommand);
}

Parser::CommandType Parser::commandType()
{
    if (splitCommands.front() == "push")
    {
        return Parser::C_PUSH;
    }
    else if (splitCommands.front() == "goto")
    {
        return Parser::C_GOTO;
    }
    else if (splitCommands.front() == "if-goto")
    {
        return Parser::C_IF;
    }
    else if (splitCommands.front() == "label")
    {
        return Parser::C_LABEL;
    }
    else if (splitCommands.front() == "call")
    {
        return Parser::C_CALL;
    }
    else if (splitCommands.front() == "function")
    {
        return Parser::C_FUNCTION;
    }
    else if (splitCommands.front() == "pop")
    {
        return Parser::C_POP;
    }
    else if (splitCommands.front() == "return")
    {
        return Parser::C_RETURN;
    }
    else if (splitCommands.front().size() < 4)
    {
        return Parser::C_ARITHMETIC;
    }
    else
    {
        return Parser::C_NONE;
    }
}

std::string Parser::arg1()
{
    if (commandType() == Parser::C_ARITHMETIC || commandType() == Parser::C_RETURN)
    {
        return splitCommands.front();
    }
    else if (commandType() == Parser::C_FUNCTION)
    {
        return splitCommands.at(1).substr(splitCommands.at(1).find_first_of(".") + 1);
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

std::string Parser::stripFront(std::string stringToStrip)
{
    size_t pos = stringToStrip.find_first_not_of(" \t");
    return stringToStrip.erase(0, pos);
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