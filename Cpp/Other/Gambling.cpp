//
//  Poker slot machine
//
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <random>
#include <chrono>
#include <windows.h>
#include <io.h>
#include <fcntl.h>

#define LANG 0  // 0 = english, 1 = russian

using namespace std;

class lData
{
public:
    string lines[2][41] =
    {
        {
            "\0",
            "  POKER SLOTS ",
            "Enter the numbers of one or two cards to re-roll, \'0\' - keep the hand\n> ",
            "You need to enter valid card numbers (e.g.: 2 5) or 0\n> ",
            "Choose cards to re-roll (\'0\' - keep the hand)\n> ",
            "// not used",
            "// not used",
            "\nEnter 5 cards (e.g.: 5 13 20 25 51)\n> ",
            "ROYAL FLUSH! Hail to the king! You get +",
            " coins",
            "Straight Flush! You get +",
            "Four of a Kind, just like that! You get +",
            "Party time, because you have a Full House! You get +",
            "It's a Flush, oh my gosh! You get +",
            "Straight! Something to brag about. You get +",
            "Here come Magi, Three of a Kind! You get +",
            "Nice, Two Pair! You get +",
            "One Pair! You get +",
            "Sorry friend, nothing this time",
            "Hand types, top-ranking first:\n",
            "  ]  Royal Flush",
            "  ]  Straight Flush",
            "  ]  Four of a Kind",
            "  ]  Full House",
            "  ]  Flush",
            "  ]  Straight",
            "  ]  Three of a Kind",
            "  ]  Two Pair",
            "  ]  One Pair",
            "\nLevel ",
            ": collect ",
            "You have: ",
            " coins\tNext level: ",
            "\nPlace your bet\n> ",
            "You need to enter at least 10 coins\n",
            "You don't have that many coins",
            "\nWhat a shame! You have not enough coins to continue :(\n",
            "You've reached level ",
            "You have rolled ",
            " times\n",
            "\nEnter \"y\" to start a new game\n> "
        },
        {
            "\0",
            "  ПОКЕР СЛОТС ",
            "Введите номера одной или двух карт для их перетасовки, \'0\' - оставить как есть\n> ",
            "Нужны корректные номера карт (например: 2 5) или 0\n> ",
            "Выберите карты для перетасовки (\'0\' - оставить как есть)\n> ",
            "// not used",
            "// not used",
            "\nВведите 5 карт (например: 5 13 20 25 51)\n> ",
            "РОЯЛ ФЛЭШ! Да здравствует король! Вы выиграли +",
            " монет\n",
            "Стрит флэш! Вы выиграли +",
            "Четыре в ряд! Вы выиграли +",
            "Принимайте гостей, у вас фул хаус! Вы выиграли +",
            "Флэш! Вы выиграли +",
            "Стрит! Этим можно и похвастаться. Вы выиграли +",
            "Три в ряд! Вы выиграли +",
            "Две пары! Скромно, но приятно. Вы выиграли +",
            "Одна пара! Вы выиграли +",
            "В этот раз не повезло",
            "Типы комбинаций, от старших к младшим:\n",
            "  ]  Роял флеш",
            "  ]  Стрит флеш",
            "  ]  Четверка",
            "  ]  Фул хаус",
            "  ]  Флеш",
            "  ]  Стрит",
            "  ]  Тройка",
            "  ]  Две пары",
            "  ]  Одна пара",
            "\nУровень ",
            ": наберите ",
            "У вас есть: ",
            " монет\tСледующий уровень: ",
            "\nДелайте вашу ставку\n> ",
            "Минимальная ставка - 10 монет\n",
            "У вас нет столько монет\n",
            "\nКакая трагедия! У вас не осталось монет для продолжения :(\n",
            "Вы достигли уровня ",
            "Сыграно ",
            " ставок\n",
            "\nВведите \"y\" чтобы начать новую игру\n> "
        }
    };
    char winMessage[2][128] = {};
    lData()
    {
        char a[] = "WINNER WINNER CHICKEN DINNER!";
        char b[] = "ПРИШЕЛ УВИДЕЛ ПОБЕДИЛ!";
        for( int i = 0; i < sizeof(a); i++) winMessage[0][i] = a[i];
        for( int i = 0; i < sizeof(b); i++) winMessage[1][i] = b[i];
    }
};

class coefData
{
public:
        // bet multipliers by hand type
    const int multipliers[9] = {1,      // one pair 42.26%
                                5,      // two pair 4.75%
                                10,     // three of a kind 2.11%
                                15,     // straight 0.39%
                                25,     // flush 0.196%
                                50,     // full house 0.1441%
                                100,    // four of a kind 0.024%
                                250,    // straight flush 0.0015%
                                1000 }; // royal flush 0.00015%
};

void printMsg( int msg = 0 )
{
    lData a;
    if(LANG == 1)
    {
        UINT WINAPI defaultCP = GetConsoleOutputCP();
        SetConsoleOutputCP(1251);
        cout << a.lines[LANG][msg];
        SetConsoleOutputCP(defaultCP);
    }
    else cout << a.lines[LANG][msg];
}

void showCursor(bool showFlag)
{
    HANDLE out = GetStdHandle(STD_OUTPUT_HANDLE);

    CONSOLE_CURSOR_INFO     cursorInfo;

    GetConsoleCursorInfo(out, &cursorInfo);
    cursorInfo.bVisible = showFlag; // set the cursor visibility
    SetConsoleCursorInfo(out, &cursorInfo);
}

void showWinnerMessage()
{
    HANDLE hstdout = GetStdHandle( STD_OUTPUT_HANDLE );
    CONSOLE_SCREEN_BUFFER_INFO csbi;
    GetConsoleScreenBufferInfo( hstdout, &csbi );
    //int outColor[] = { 8, 2, 3, 6, 4, 5 };
    int outColor[] = { 9, 11, 10, 12, 14, 13 };
    UINT WINAPI defaultCP = GetConsoleOutputCP();
    if( LANG == 1 ) SetConsoleOutputCP(1251);
    lData a;
    int curCol = 0, i = 0;
    for( ;; )
    {
        SetConsoleTextAttribute( hstdout, outColor[curCol] );
        cout << a.winMessage[LANG][i];
        i++;
        if( a.winMessage[LANG][i] == '\0') break;
        if( ++curCol == sizeof(outColor) / sizeof(outColor[0]) ) curCol = 0;
    }
    cout << endl;
    SetConsoleOutputCP( defaultCP );
    SetConsoleTextAttribute( hstdout, csbi.wAttributes );
}

class Player
{
public:
    Player(int nCoins = 500)
    {
        coins = nCoins;
    }
    int getCoins()
    {
        return coins;
    }
    void addCoins(int nCoins)
    {
        coins += nCoins;
    }
protected:
    int     coins;
};

class Card
{
public:
    char suit, rnk;
    Card() { suit = 3; rnk = 12; }
    void displayCard(int s, int r)
    {
        HANDLE hstdout = GetStdHandle( STD_OUTPUT_HANDLE );
        // remember default output colors
        CONSOLE_SCREEN_BUFFER_INFO csbi;
        GetConsoleScreenBufferInfo( hstdout, &csbi );
        wchar_t crd[4] = { rankS[r][0], rankS[r][1], suitS[s], '\0' };
        int cardColors[4] = { 0xC, 0xE, 0xA, 0xB };  // hearts, diamonds, clubs, spades
        // set card color by suit
        SetConsoleTextAttribute( hstdout, cardColors[s] );
        _setmode(_fileno(stdout), _O_U16TEXT);
        wcout << crd[0] << crd[1] << crd[2] << crd[3];
        // reset output colors to default
        _setmode(_fileno(stdout), _O_TEXT);
        SetConsoleTextAttribute( hstdout, csbi.wAttributes );
    }
    void getCard()
    {
        displayCard(suit, rnk);
    }
protected:
    wchar_t suitS[4] = { L'\u2665', L'\u2666', L'\u2663', L'\u2660' };  // 3 = hearts, 4 = diamonds, 5 = clubs, 6 = spades
    wchar_t rankS[13][2] = { { ' ', '2' }, { ' ', '3' }, { ' ', '4' }, { ' ', '5' }, { ' ', '6' }, { ' ', '7' }, { ' ', '8' }, { ' ', '9' }, { '1', '0' },
                          { ' ', 'J' }, { ' ', 'Q' }, { ' ', 'K' }, { ' ', 'A' } };
};

void showGameTitle()
{
    Card  crd;
    HANDLE hstdout = GetStdHandle( STD_OUTPUT_HANDLE );
    // remember default output colors
    CONSOLE_SCREEN_BUFFER_INFO csbi;
    GetConsoleScreenBufferInfo( hstdout, &csbi );
    crd.displayCard(0, 12); crd.displayCard(1, 11); crd.displayCard(3, 10); crd.displayCard(2, 9);
    SetConsoleTextAttribute( hstdout, 0xf );
    printMsg(1);
    // reset output colors to default
    SetConsoleTextAttribute( hstdout, csbi.wAttributes );
    crd.displayCard(2, 9); crd.displayCard(3, 10); crd.displayCard(1, 11); crd.displayCard(0, 12);
    cout << endl;
}

class SlotMachine
{
public:
    bool firstTime = true;

    SlotMachine()
    {
        // initialize slots
        for(int i = 0; i < 5; i++) { slot[i].suit = 0; slot[i].rnk = 0; }
        // get random seed from timer
        typedef std::chrono::high_resolution_clock Time;
        typedef std::chrono::microseconds mcs;
        Time::time_point t = Time::now();
        mcs d = std::chrono::duration_cast<mcs>(t.time_since_epoch());
        double seed = d.count() % 1000000;
        // initialize random number engine
        rng = new mt19937(seed);
    }
    ~SlotMachine()
    {
        delete rng;
    }
    int newRoll(int inputCoins)
    {
        // set up random number distribution range
        uniform_int_distribution<int> uni(0, deckSize - 1);
        // roll'em!
        int rndNums[5] = { 0, 0, 0, 0, 0 };
        bool haveEquals;
        for( int i = 0; i < 5; i++)
        {
            do
            {
                rndNums[i] = uni(*rng);
                haveEquals = false;
                for( int j = 0; j < i; j++)
                {
                    if( rndNums[i] == rndNums[j] ) { haveEquals = true; }
                }
            } while( haveEquals );
        }
        setSlots(rndNums);
        getRoll();
        if( firstTime ) printMsg(2);
        else printMsg(4);
        int reRoll[2];
        for(;;)
        {
            reRoll[0] = -1; reRoll[1] = -1;
            scanf("%d", &reRoll[0]);
            char tmp[16]={0x0};
            if( fgets(tmp, sizeof(tmp), stdin) != NULL )
            {
                for( int i = 0; i < sizeof(tmp); i++ )
                {
                    if( (tmp[i] >= 48) && (tmp[i] <= 57) ) { reRoll[1] = tmp[i] - 48; }
                }
            }
            fflush(stdin);
            if( ((reRoll[0] >= 0) && (reRoll[0] <=5)) && ((reRoll[1] == -1) || ((reRoll[1] > 0) && (reRoll[1] <=5)))  ) { break; }
            printMsg(3);
        }
        if( (reRoll[0] > 0) && (reRoll[1] == -1) )
        {
            reRoll[0]--;
            int newSlot;
            do
            {
                newSlot = uni(*rng);
                haveEquals = false;
                for( int i = 0; i < 5; i++)
                {
                    if( newSlot == rndNums[i] ) { haveEquals = true; }
                }
            } while( haveEquals );
            rndNums[reRoll[0]] = newSlot;
            setSlots(rndNums);
            getRoll();
        }
        if( (reRoll[0] > 0) && (reRoll[1] > 0) )
        {
            reRoll[0]--; reRoll[1]--;
            int newSlot1, newSlot2;
            do
            {
                newSlot1 = uni(*rng);
                newSlot2 = uni(*rng);
                haveEquals = false;
                for( int i = 0; i < 5; i++)
                {
                    if( newSlot1 == newSlot2 ) { haveEquals = true; }
                    if( newSlot1 == rndNums[i] ) { haveEquals = true; }
                    if( newSlot2 == rndNums[i] ) { haveEquals = true; }
                }
            } while( haveEquals );
            rndNums[reRoll[0]] = newSlot1;
            rndNums[reRoll[1]] = newSlot2;
            setSlots(rndNums);
            getRoll();
        }
        return getPrize(inputCoins);
    }
    void testRoll()
    {
        for( int i = 0; i < 4; i++)
        {
            for( int j = 0; j < 13; j++)
            {
                if( i * 13 + j < 10 ) { cout << ' ' << i * 13 + j << ':'; }
                else { cout << i * 13 + j << ':'; }
                slot[0].displayCard(i, j);
                cout << '\t';
            }
            cout << endl;
        }
        int rndNums[5] = { 0, 0, 0, 0, 0 };
        printMsg(7);
        for(int i = 0; i < 5; i++)
        {
            scanf("%d", &rndNums[i]);
        }
        fflush(stdin);
        setSlots(rndNums);
        getRoll();
        getPrize(1);
    }
protected:
    const int   deckSize = 52, sleepTime = 300;
    Card        slot[5];
    mt19937*    rng;
    coefData    coefs;

    void setSlots(int cardNums[5])
    {
        for( int i = 0; i < 5; i++)
        {
            slot[i].suit = cardNums[i] / 13;
            slot[i].rnk = cardNums[i] % 13;
        }
    }
    void setCardColor(int cardSuit)
    {
        HANDLE hstdout = GetStdHandle( STD_OUTPUT_HANDLE );
        int cardColors[4] = { 0xD, 0xE, 0xB, 0xA };  // hearts, diamonds, clubs, spades
        SetConsoleTextAttribute( hstdout, cardColors[cardSuit] );
    }
    void getRoll()
    {
        showCursor(false);
        cout << "[";
        Sleep(sleepTime);
        cout << " "; slot[0].getCard();
        Sleep(sleepTime);
        cout << " "; slot[1].getCard();
        Sleep(sleepTime);
        cout << " "; slot[2].getCard();
        Sleep(sleepTime);
        cout << " "; slot[3].getCard();
        Sleep(sleepTime);
        cout << " "; slot[4].getCard();
        cout << "  ] " << endl;
        Sleep(sleepTime);
        showCursor(true);
    }
    int getPrize(int prizeCoins)
    {
        // create a copy of slots sorted by rank
        int     sortedSlots[5] = { slot[0].rnk, slot[1].rnk, slot[2].rnk, slot[3].rnk, slot[4].rnk };
        int     slotNum, tempSlot;
        for( int i = 0; i < 4; i++)
        {
            slotNum = i;
            for( int j = i + 1; j < 5; j++)
            {
                if( sortedSlots[j] < sortedSlots[slotNum] ) { slotNum = j; }
            }
            tempSlot = sortedSlots[slotNum];
            sortedSlots[slotNum] = sortedSlots[i];
            sortedSlots[i] = tempSlot;
        }
        // royal flush
        bool    haveStraight = false,
                haveFlush = false;
        if( ((sortedSlots[0] + 1 == sortedSlots[1]) && (sortedSlots[1] + 1 == sortedSlots[2]) && (sortedSlots[2] + 1 == sortedSlots[3]) && (sortedSlots[3] + 1 == sortedSlots[4])) ||  // straight
            ((sortedSlots[4] == 12) && (sortedSlots[0] == 0) && (sortedSlots[1] == 1) && (sortedSlots[2] == 2) && (sortedSlots[3] == 3)) ) { haveStraight = true; }  // "wheel" straight
        if( (slot[0].suit == slot[1].suit) && (slot[0].suit == slot[2].suit) && (slot[0].suit == slot[3].suit) && (slot[0].suit == slot[4].suit) ) { haveFlush = true; }  // flush
        if( haveStraight && haveFlush && (sortedSlots[3] == 11) && (sortedSlots[4] == 12) )
        {
            prizeCoins *= coefs.multipliers[8];
            printMsg(8);
            cout << prizeCoins;
            printMsg(9);
            return prizeCoins;
        }
        // straight flush
        if( haveStraight && haveFlush )
        {
            prizeCoins *= coefs.multipliers[7];
            printMsg(10);
            cout << prizeCoins;
            printMsg(9);
            return prizeCoins;
        }
        // four of a kind
        if( ((sortedSlots[0] == sortedSlots[1]) && (sortedSlots[0] == sortedSlots[2]) && (sortedSlots[0] == sortedSlots[3])) ||
            ((sortedSlots[1] == sortedSlots[2]) && (sortedSlots[1] == sortedSlots[3]) && (sortedSlots[1] == sortedSlots[4])) )
        {
            prizeCoins *= coefs.multipliers[6];
            printMsg(11);
            cout << prizeCoins;
            printMsg(9);
            return prizeCoins;
        }
        // full house
        if( ((sortedSlots[0] == sortedSlots[1]) && (sortedSlots[0] == sortedSlots[2]) && (sortedSlots[3] == sortedSlots[4])) ||
            ((sortedSlots[0] == sortedSlots[1]) && (sortedSlots[2] == sortedSlots[3]) && (sortedSlots[2] == sortedSlots[4])) )
        {
            prizeCoins *= coefs.multipliers[5];
            printMsg(12);
            cout << prizeCoins;
            printMsg(9);
            return prizeCoins;
        }
        // flush
        if( haveFlush )
        {
            prizeCoins *= coefs.multipliers[4];
            printMsg(13);
            cout << prizeCoins;
            printMsg(9);
            return prizeCoins;
        }
        // straight
        if( haveStraight )  // "wheel" straight
        {
            prizeCoins *= coefs.multipliers[3];
            printMsg(14);
            cout << prizeCoins;
            printMsg(9);
            return prizeCoins;
        }
        // three of a kind
        bool    haveHand = false;
        for( int i = 0; i < 3; i++ )
        {
            if( (sortedSlots[i] == sortedSlots[i + 1]) && (sortedSlots[i] == sortedSlots[i + 2]) )
            {
                haveHand = true;
                break;
            }
        }
        if( haveHand )
        {
            prizeCoins *= coefs.multipliers[2];
            printMsg(15);
            cout << prizeCoins;
            printMsg(9);
            return prizeCoins;
        }
        // two pair
        bool    firstPair = false;
        int     firstPairRank = -1;
        for( int i = 0; i < 4; i++ )
        {
            for( int j = i + 1; j < 5; j++)
            {
                if( slot[i].rnk == slot[j].rnk )
                {
                    firstPair = true;
                    firstPairRank = slot[i].rnk;
                    break;
                }
            }
        }
        if( firstPair )
        {
            for( int i = 0; i < 4; i++ )
            {
                for( int j = i + 1; j < 5; j++)
                {
                    if( (slot[i].rnk == slot[j].rnk) && (slot[i].rnk != firstPairRank) ) { haveHand = true; }
                }
            }
        }
        if( haveHand )
        {
            prizeCoins *= coefs.multipliers[1];
            printMsg(16);
            cout << prizeCoins;
            printMsg(9);
            return prizeCoins;
        }
        // one pair
        for( int i = 0; i < 4; i++ )
        {
            for( int j = i + 1; j < 5; j++)
            {
                if( slot[i].rnk == slot[j].rnk ) { haveHand = true; }
            }
        }
        if( haveHand )
        {
            prizeCoins *= coefs.multipliers[0];
            printMsg(17);
            cout << prizeCoins;
            printMsg(9);
            return prizeCoins;
        }
        printMsg(18);
        cout << endl;
        return 0;
    }
};

void showLegend()
{
    Card crd;
    coefData coefs;
    printMsg(19);
    cout << "[ ";
    crd.displayCard(1, 12); cout << " "; crd.displayCard(1, 11); cout << " "; crd.displayCard(1, 10); cout << " "; crd.displayCard(1, 9); cout << " "; crd.displayCard(1, 8);
    printMsg(20);
    cout << "\t x" << coefs.multipliers[8] << '\n';
    cout << "[ ";
    crd.displayCard(2, 10); cout << " "; crd.displayCard(2, 9); cout << " "; crd.displayCard(2, 8); cout << " "; crd.displayCard(2, 7); cout << " "; crd.displayCard(2, 6);
    printMsg(21);
    cout << "\t x" << coefs.multipliers[7] << '\n';
    cout << "[ ";
    crd.displayCard(3, 7); cout << " "; crd.displayCard(2, 7); cout << " "; crd.displayCard(1, 7); cout << " "; crd.displayCard(0, 7); cout << " "; crd.displayCard(0, 9);
    printMsg(22);
    cout << "\t x" << coefs.multipliers[6] << '\n';
    cout << "[ ";
    crd.displayCard(2, 1); cout << " "; crd.displayCard(3, 1); cout << " "; crd.displayCard(1, 1); cout << " "; crd.displayCard(2, 4); cout << " "; crd.displayCard(0, 4);
    printMsg(23);
    cout << "\t x" << coefs.multipliers[5] << '\n';
    cout << "[ ";
    crd.displayCard(2, 10); cout << " "; crd.displayCard(2, 8); cout << " "; crd.displayCard(2, 5); cout << " "; crd.displayCard(2, 4); cout << " "; crd.displayCard(2, 2);
    printMsg(24);
    cout << "\t\t x" << coefs.multipliers[4] << '\n';
    cout << "[ ";
    crd.displayCard(2, 10); cout << " "; crd.displayCard(3, 9); cout << " "; crd.displayCard(3, 8); cout << " "; crd.displayCard(0, 7); cout << " "; crd.displayCard(0, 6);
    printMsg(25);
    cout << "\t\t x" << coefs.multipliers[3] << '\n';
    cout << "[ ";
    crd.displayCard(1, 0); cout << " "; crd.displayCard(3, 0); cout << " "; crd.displayCard(2, 0); cout << " "; crd.displayCard(3, 11); cout << " "; crd.displayCard(0, 4);
    printMsg(26);
    cout << "\t x" << coefs.multipliers[2] << '\n';
    cout << "[ ";
    crd.displayCard(0, 9); cout << " "; crd.displayCard(2, 9); cout << " "; crd.displayCard(2, 2); cout << " "; crd.displayCard(3, 2); cout << " "; crd.displayCard(0, 7);
    printMsg(27);
    cout << "\t x" << coefs.multipliers[1] << '\n';
    cout << "[ ";
    crd.displayCard(0, 2); cout << " "; crd.displayCard(3, 2); cout << " "; crd.displayCard(3, 11); cout << " "; crd.displayCard(1, 8); cout << " "; crd.displayCard(3, 3);
    printMsg(28);
    cout << "\t x" << coefs.multipliers[0] << '\n';
}

int main(int nNumberofArgs, char* pszArgs[])
{
    SlotMachine machine;
    Player*     player1;
    char        playerInput, playMore;
    int         coins, rollsNumber, curTopScore;
    int         topScore[] = { 1000, 3000, 10000, 50000, 1000000 };
    for( ;; )
    {
        player1 = new Player;
        rollsNumber = 0;
        curTopScore = 0;
        system("CLS");
        showGameTitle();
        printMsg(29);
        cout << curTopScore + 1;
        printMsg(30);
        cout << topScore[curTopScore];
        printMsg(9);
        cout << endl << endl;
        system("pause");
        for(;;)
        {
            coins = -1;
            playMore = '\0';
            playerInput = '\0';
            system("CLS");
            showLegend();
            cout << "====================================================\n";
            printMsg(31);
            cout << player1->getCoins();
            printMsg(32);
            cout << topScore[curTopScore];
            printMsg(9);
            printMsg(33);
            scanf("%d", &coins);
            if( coins < 0 ) { scanf("%1c", &playerInput); }
            fflush(stdin);
            if( playerInput == 'q')
            {
                playMore = 'n';
                break;
            }
            if( playerInput == 't' )
            {
                system("CLS");
                machine.testRoll();
            }
            if( coins > 0 )
            {
                if( coins < 10 ) printMsg(34);
                if( coins > player1->getCoins() ) printMsg(35);
            }
            if( (coins >= 10) && (coins <= player1->getCoins()) )
            {
                player1->addCoins(-coins);
                player1->addCoins(machine.newRoll(coins));
                machine.firstTime = false;
                rollsNumber++;
                if( player1->getCoins() < 10 )
                {
                    printMsg(36);
                    break;
                }
                if( player1->getCoins() >= topScore[curTopScore] )
                {
                    cout << endl;
                    system("pause");
                    system("CLS");
                    showGameTitle();
                    cout << endl;
                    do
                    {
                        curTopScore++;
                        printMsg(37);
                        cout << curTopScore + 1 << "!\n";
                    } while( (player1->getCoins() >= topScore[curTopScore]) && (curTopScore < sizeof(topScore) / sizeof(topScore[0])) );
                    if( curTopScore == sizeof(topScore) / sizeof(topScore[0]) )
                    {
                        printMsg(31);
                        cout << player1->getCoins();
                        printMsg(9);
                        showWinnerMessage();
                        break;
                    }
                    printMsg(29);
                    cout << curTopScore + 1;
                    printMsg(30);
                    cout << topScore[curTopScore];
                    printMsg(9);
                }
            }
            cout << endl;
            system("pause");
        }
        printMsg(38);
        cout << rollsNumber;
        printMsg(39);
        delete player1;
        if( playMore == 'n' ) { break; }
        printMsg(40);
        scanf("%c", &playMore);
        if( playMore != 'y') { break; }
        system("CLS");
    }
    return 0;
}
