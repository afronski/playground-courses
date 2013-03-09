import java.util.Arrays;

public class Board {
    private final short board [][];
    private final int N;
    private int manhattan = -1;

    // Construct a board from an N-by-N array of blocks
    // (where blocks[i][j] = block in row i, column j).
    public Board(int[][] blocks) {
        N = blocks[0].length;

        board = copyFromInts(blocks);
    }

    // Construct a board from an N-by-N array of blocks
    // (where blocks[i][j] = block in row i, column j).
    private Board(short[][] blocks) {
        N = blocks[0].length;

        board = copy(blocks);
    }

    // Board dimension N.
    public int dimension() {
        return N;
    }

    // Number of blocks out of place.
    public int hamming() {
        int count = 0;
        int expectation = 1;

        for (int i = 0; i < N; ++i) {
            for (int j = 0; j < N; ++j) {
                if (board[i][j] != expectation++) {
                    count++;
                }
            }
        }

        return count - 1;
    }

    // Sum of Manhattan distances between blocks and goal.
    public int manhattan() {
        if (manhattan >= 0) {
            return manhattan;
        }

        int sum = 0, value = 0;
        int goalRow, goalCol;

        for (int i = 0; i < N; ++i) {
            for (int j = 0; j < N; ++j) {
                value = board[i][j];

                if (value == 0) {
                    continue;
                }

                goalRow = (value - 1) / N;
                goalCol = (value - 1) % N;

                sum += Math.abs(i - goalRow) + Math.abs(j - goalCol);
            }
        }

        manhattan = sum;
        return sum;
    }

    // Is this board the goal board?
    public boolean isGoal() {
        int length = N - 1;

        if (board[length][length] != 0) {
            return false;
        }

        int value = 1;

        for (int i = 0; i < N; ++i) {
            for (int j = 0; j < N; ++j) {
                if (board[i][j] != value++ && (i != length || j != length)) {
                    return false;
                }
            }
        }

        return true;
    }

    // A board obtained by exchanging two adjacent blocks in the same row.
    public Board twin() {
        short[][] copy = copy(board);

        if (N <= 1) {
            return new Board(copy);
        }

        int i = 0, j = 0;
        short value = 0, lastValue = board[0][0];

        zerosearch:
        for (i = 0; i < N; ++i) {
            for (j = 0; j < N; ++j) {
                value = board[i][j];

                if (value != 0 && lastValue != 0 && j > 0) {
                    break zerosearch;
                }

                lastValue = value;
            }
        }

        copy[i][j] = lastValue;
        copy[i][j - 1] = value;

        return new Board(copy);
    }

    // Does this board equal y?
    @Override
    public boolean equals(Object y) {
        if (y == this) {
            return true;
        }

        if (y == null) {
            return false;
        }

        if (y.getClass() != this.getClass()) {
            return false;
        }

        Board that = (Board) y;

        if (this.board.length != that.board.length) {
            return false;
        }

        for (int i = 0; i < N; ++i) {
            if (!Arrays.equals(this.board[i], that.board[i])) {
                return false;
            }
        }

        return true;
    }

    private short[][] copyFromInts(int[][] input) {
        short output[][] = new short [N][N];

        for (int i = 0; i < N; ++i) {
            for (int j = 0; j < N; ++j) {
                output[i][j] = (short) input[i][j];
            }
        }

        return output;
    }

    private short[][] copy(short[][] input) {
        short output[][] = new short [N][N];

        for (int i = 0; i < N; ++i) {
            for (int j = 0; j < N; ++j) {
                output[i][j] = input[i][j];
            }
        }

        return output;
    }

    private short[][] makeMove(short[][] array,
                            int fromX,
                            int fromY,
                            int toX,
                            int toY) {
        short[][] copy = copy(array);

        short replacement = copy[toX][toY];
        copy[toX][toY] = copy[fromX][fromY];
        copy[fromX][fromY] = replacement;

        return copy;
    }

    // All neighboring boards.
    public Iterable<Board> neighbors() {
        Queue<Board> q = new Queue<Board>();

        int i = 0, j = 0;

        zerosearch:
        for (i = 0; i < N; ++i) {
            for (j = 0; j < N; ++j) {
                if (board[i][j] == 0) {
                    break zerosearch;
                }
            }
        }

        if (i > 0) {
            q.enqueue(new Board(makeMove(board, i, j, i - 1, j)));
        }

        if (i < N - 1) {
            q.enqueue(new Board(makeMove(board, i, j, i + 1, j)));
        }

        if (j > 0) {
            q.enqueue(new Board(makeMove(board, i, j, i, j - 1)));
        }

        if (j < N - 1) {
            q.enqueue(new Board(makeMove(board, i, j, i, j + 1)));
        }

        return q;
    }

    // String representation of the board (in the output format specified below).
    @Override
    public String toString() {
        StringBuilder s = new StringBuilder();
        s.append(N + "\n");

        for (int i = 0; i < N; i++) {
            for (int j = 0; j < N; j++) {
                s.append(String.format("%2d ", board[i][j]));
            }

            s.append("\n");
        }
        return s.toString();
    }
}