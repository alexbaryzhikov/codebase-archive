import java.util.List;
import game.Board;
import game.Position;
import mcts.MonteCarloTreeSearch;
import mcts.State;
import mcts.UCT;
import mcts.Tree;

public class Tests {
    private Tree gameTree;
    private MonteCarloTreeSearch mcts;

    public void initGameTree() {
        gameTree = new Tree();
        mcts = new MonteCarloTreeSearch();
    }

    public void givenStats_whenGetUCTForNode_thenUCTMatchesWithManualData() {
        double uctValue = 15.79;
        assertEquals(UCT.uctValue(600, 300, 20), uctValue, 0.01);
    }

    public void giveninitBoardState_whenGetAllPossibleStates_thenNonEmptyList() {
        State initState = gameTree.getRoot().getState();
        List<State> possibleStates = initState.getAllPossibleStates();
        assertTrue(possibleStates.size() > 0);
    }

    public void givenEmptyBoard_whenPerformMove_thenLessAvailablePossitions() {
        Board board = new Board();
        int initAvailablePositions = board.getEmptyPositions().size();
        board.performMove(Board.P1, new Position(1, 1));
        int availablePositions = board.getEmptyPositions().size();
        assertTrue(initAvailablePositions > availablePositions);
    }

    public void givenEmptyBoard_whenSimulateInterAIPlay_thenGameDraw() {
        Board board = new Board();
        int player = Board.P1;
        int totalMoves = Board.DEFAULT_BOARD_SIZE * Board.DEFAULT_BOARD_SIZE;
        for (int i = 0; i < totalMoves; i++) {
            board = mcts.findNextMove(board, player);
            if (board.checkStatus() != -1) {
                break;
            }
            player = 3 - player;
        }
        int winStatus = board.checkStatus();
        assertEquals(winStatus, Board.DRAW);
    }

    public void givenEmptyBoard_whenLevel1VsLevel3_thenLevel3WinsOrDraw() {
        Board board = new Board();
        MonteCarloTreeSearch mcts1 = new MonteCarloTreeSearch();
        mcts1.setLevel(1);
        MonteCarloTreeSearch mcts3 = new MonteCarloTreeSearch();
        mcts3.setLevel(3);

        int player = Board.P1;
        int totalMoves = Board.DEFAULT_BOARD_SIZE * Board.DEFAULT_BOARD_SIZE;
        for (int i = 0; i < totalMoves; i++) {
            if (player == Board.P1)
                board = mcts3.findNextMove(board, player);
            else
                board = mcts1.findNextMove(board, player);

            if (board.checkStatus() != -1) {
                break;
            }
            player = 3 - player;
        }
        int winStatus = board.checkStatus();
        assertTrue(winStatus == Board.DRAW || winStatus == Board.P1);
    }

    private void assertEquals(double val1, double val2, double epsilon) throws AssertionError {
        if (Math.abs(val1 - val2) > epsilon)
            throw new AssertionError();
    }

    private void assertEquals(int val1, int val2) throws AssertionError {
        if (val1 != val2)
            throw new AssertionError();
    }

    private void assertTrue(boolean test) throws AssertionError {
        if (!test)
            throw new AssertionError();
    }
}
