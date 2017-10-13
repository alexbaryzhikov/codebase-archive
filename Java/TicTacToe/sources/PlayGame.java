import java.util.List;
import java.util.Scanner;
import game.Board;
import game.Position;
import mcts.MonteCarloTreeSearch;

public class PlayGame {

    public static void main(String[] args) {
        // RunTests();

        // Play a game vs AI
        boolean playnext = true;
        int player, humanPlayer;
        Scanner cin = new Scanner(System.in); 
        Board board;
        MonteCarloTreeSearch agent;

        while (playnext) {
            board = new Board();
            agent = new MonteCarloTreeSearch();
            humanPlayer = (int) (Math.random() * 2) + 1;
            player = Board.P1;

            while (board.checkStatus() == -1) {
                board.printBoard();
                System.out.println();
                if (player == humanPlayer) {
                    List<Position> p = board.getEmptyPositions();
                    if (p.size() == 1) {
                        board = new Board(board);
                        board.performMove(player, p.get(0));
                    } else
                        board = playerMove(board, player);
                    System.out.println();
                } else
                    board = agent.findNextMove(board, player);
                player = 3 - player;
            }

            board.printBoard();
            System.out.println();

            switch (board.checkStatus()) {
            case 0:
                System.out.println("DRAW");
                break;
            case 1:
                System.out.println("YOU " + (humanPlayer == 1 ? "WIN" : "LOSE"));
                break;
            case 2:
                System.out.println("YOU " + (humanPlayer == 2 ? "WIN" : "LOSE"));
                break;
            default:
                System.out.println("UNEXPECTED STATUS");
                break;
            }

            System.out.print("Play next? (y/n) ");
            if (cin.nextLine().charAt(0) != 'y')
                playnext = false;
            System.out.println();
        }
    }

    private static Board playerMove(Board board, int player) {
        int x, y;
        Scanner cin = new Scanner(System.in); 
        System.out.print(">> ");
        x = cin.nextInt() - 1;
        y = cin.nextInt() - 1;
        Position p = new Position(x, y);
        Board res = new Board(board);
        res.performMove(player, p);
        return res;
    }

    private static void RunTests() {
        Tests tests = new Tests();
        tests.initGameTree();
        tests.givenStats_whenGetUCTForNode_thenUCTMatchesWithManualData();
        tests.giveninitBoardState_whenGetAllPossibleStates_thenNonEmptyList();
        tests.givenEmptyBoard_whenPerformMove_thenLessAvailablePossitions();
        tests.givenEmptyBoard_whenLevel1VsLevel3_thenLevel3WinsOrDraw();
        System.out.println("tests pass");
    }
}
