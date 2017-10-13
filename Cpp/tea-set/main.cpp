//
//  Tea-set puzzle solving by bruteforcing.
//  We have the following setup:
//  C - A
//  C C B
//  Task: switch A and B with the least possible turns.
//  Conditions:
//  1) Element can be moved to adjacent free cell
//  2) No diagonal movement
//  3) Element can't be moved over another element
//  4) A cell can contain only one element at any given time
#include <cstdio>
#include <cstdlib>
#include <iostream>
using namespace std;
// declaring global variables
bool        solved = false;
int         turnsNumber = 0,
            emptyCell[2],
            currentTurnOrder[18] = {},
            lastTurnOrder[18] = {};
char        board[2][3] = {};

// =====================
// Board procedures
// =====================

void defaultArray()
{
    board[0][0] = 'C';
    board[0][1] = '-';
    board[0][2] = 'A';
    board[1][0] = 'C';
    board[1][1] = 'C';
    board[1][2] = 'B';
    emptyCell[0] = 0;
    emptyCell[1] = 1;
}

string showCurrentTurnOrder()
{
    string result;
    for(int i = 1; i <= turnsNumber; ++i)
        result += static_cast<char>(currentTurnOrder[i]+48);
    return result;
}

void moveEmptyCell(int moveDir)
{
    // up
    if(moveDir == 0)
    {
        if((emptyCell[0] - 1) >= 0)
        {
            board[emptyCell[0]][emptyCell[1]] = board[emptyCell[0] - 1][emptyCell[1]];
            --emptyCell[0];
            board[emptyCell[0]][emptyCell[1]] = '-';
        }
        else
        {
            cout << "> wrong move: out of boundaries, turn order: " << showCurrentTurnOrder() << endl;
        }
    }
    // right
    if(moveDir == 1)
    {
        if((emptyCell[1] + 1) <= 2)
        {
            board[emptyCell[0]][emptyCell[1]] = board[emptyCell[0]][emptyCell[1] + 1];
            ++emptyCell[1];
            board[emptyCell[0]][emptyCell[1]] = '-';
        }
        else
        {
            cout << "> wrong move: out of boundaries, turn order: " << showCurrentTurnOrder() << endl;
        }
    }
    // down
    if(moveDir == 2)
    {
        if((emptyCell[0] + 1) <= 1)
        {
            board[emptyCell[0]][emptyCell[1]] = board[emptyCell[0] + 1][emptyCell[1]];
            ++emptyCell[0];
            board[emptyCell[0]][emptyCell[1]] = '-';
        }
        else
        {
            cout << "> wrong move: out of boundaries, turn order: " << showCurrentTurnOrder() << endl;
        }
    }
    // left
    if(moveDir == 3)
    {
        if((emptyCell[1] - 1) >= 0)
        {
            board[emptyCell[0]][emptyCell[1]] = board[emptyCell[0]][emptyCell[1] - 1];
            --emptyCell[1];
            board[emptyCell[0]][emptyCell[1]] = '-';
        }
        else
        {
            cout << "> wrong move: out of boundaries, turn order: " << showCurrentTurnOrder() << endl;
        }
    }
}

void updateArray()
{
    defaultArray();
    for(int i = 1; i <= turnsNumber; ++i)
    {
        // move empty cell to the given direction
        moveEmptyCell(currentTurnOrder[i]);
    }
}

bool checkSolution()
{
    if((board[0][2] == 'B') && (board[1][2] == 'A')) return true;
    return false;
}

// =====================
// Turn order procedures
// =====================

int makeNextTurn(int prevMove = 99)
{
    switch(prevMove)
    {
        case 99 :
            if(emptyCell[0] - 1 >= 0)
            {
                --emptyCell[0];
                return 0;  // up
            }
            if(emptyCell[1] + 1 <= 2)
            {
                ++emptyCell[1];
                return 1;  // right
            }
            if(emptyCell[0] + 1 <= 1)
            {
                ++emptyCell[0];
                return 2;  // down
            }
            if(emptyCell[1] - 1 >= 0)
            {
                --emptyCell[1];
                return 3;  // left
            }
            break;
        case 0 :
            if(emptyCell[1] + 1 <= 2)
            {
                ++emptyCell[1];
                return 1;  // right
            }
            if(emptyCell[0] + 1 <= 1)
            {
                ++emptyCell[0];
                return 2;  // down
            }
            if(emptyCell[1] - 1 >= 0)
            {
                --emptyCell[1];
                return 3;  // left
            }
            break;
        case 1 :
            if(emptyCell[0] + 1 <= 1)
            {
                ++emptyCell[0];
                return 2;  // down
            }
            if(emptyCell[1] - 1 >= 0)
            {
                --emptyCell[1];
                return 3;  // left
            }
            break;
        case 2 :
            if(emptyCell[1] - 1 >= 0)
            {
                --emptyCell[1];
                return 3;  // left
            }
            break;
    }
    return prevMove;
}

void updateEmptyCellPos()
{
    // move empty cell to the second to last position of lastTurnOrder
    for(int i = 1; i < turnsNumber; ++i)
    {
        if(lastTurnOrder[i] == 0) --emptyCell[0];
        if(lastTurnOrder[i] == 1) ++emptyCell[1];
        if(lastTurnOrder[i] == 2) ++emptyCell[0];
        if(lastTurnOrder[i] == 3) --emptyCell[1];
    }
}

void rewindEmptyCell(int turn)
{
    if(turn == 0) ++emptyCell[0];
    if(turn == 1) --emptyCell[1];
    if(turn == 2) --emptyCell[0];
    if(turn == 3) ++emptyCell[1];
}

void promoteCurrentTurnOrder(bool initialCall = false)
{
    static int turnPos;
    if(initialCall) turnPos = turnsNumber;
    // try to promote the turn # turnPos (initially the last turn in lastTurnOrder)
    currentTurnOrder[turnPos] = makeNextTurn(lastTurnOrder[turnPos]);
    if(currentTurnOrder[turnPos] == lastTurnOrder[turnPos]) // failed to promote
    {
        if(--turnPos <= 0) return;  // if no more turns, then currentTurnOrder remains unchanged
        rewindEmptyCell(lastTurnOrder[turnPos]); // undo emptyCell previous move
        promoteCurrentTurnOrder();  // try to promote previous turn
    }
    // fill the rest of the turn order with initial turns
    if(initialCall && (turnPos > 0))
        for(int i = turnPos + 1; i <= turnsNumber; ++i)
            currentTurnOrder[i] = makeNextTurn();
}

bool areTurnOrdersEqual()
{
    for(int i = 0; i <= 17; ++i)
    {
        if(currentTurnOrder[i] != lastTurnOrder[i]) return false;
    }
    return true;
}

bool createNextTurnOrder()
{
    emptyCell[0] = 0;
    emptyCell[1] = 1;
    if(lastTurnOrder[0] == 0)  // if it's the first call since turnsNumber incremented
    {
        // generate initial turn order and write it to currentTurnOrder
        for(int i = 1; i <= turnsNumber; ++i) currentTurnOrder[i] = makeNextTurn();
    }
    else
    {
        updateEmptyCellPos();
        promoteCurrentTurnOrder(true);  // try to promote currentTurnOrder
        if(areTurnOrdersEqual()) return false;  // failed to promote currentTurnOrder
    }
    for(int i = 0; i <= 17; ++i) lastTurnOrder[i] = currentTurnOrder[i];
    return true;
}

// =====================
// Main
// =====================

int main(int nNumberofArgs, char* pszArgs[])
{
    currentTurnOrder[0] = 1;
    // keep adding turns until at least
    // one solution is found
    while(!solved)
    {
        if(++turnsNumber > 17) break;
        cout << "> turnsNumber = " << turnsNumber << endl;
        lastTurnOrder[0] = 0;  // start fresh
        // go through all possible turn orders
        // of given number of turns
        for(;;)
        {
            if(!createNextTurnOrder()) break;
            updateArray();
            if(checkSolution())
            {
                cout << "> solution found " << showCurrentTurnOrder() << endl;
                cout << "Press ENTER to continue";
                solved = true;
                cin.get();
                cout << "> resuming sequence" << endl;
            }
        }
    }
    cout << "> the sequence is complete " << endl;
    cin.get();
    return 0;
}

