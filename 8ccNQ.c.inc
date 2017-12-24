R"(int printf();

void print_board(int board[][8]) {
  int i;
  for (i = 0; i < 8; i++) {
    int j;
    for (j = 0; j < 8; j++) {
      if (board[i][j])
        printf("Q ");
      else
        printf(". ");
    }
    printf("\n");
  }
  printf("\n\n");
}
int conflict(int board[][8], int row, int col) {
  int i;
  for(i = 0; i < row; i++) {
    if (board[i][col])
      return 1;
    int j;
    j = row - i;
    if (0 < col - j + 1)
      if (board[i][col - j])
        return 1;
    if (col + j < 8)
      if (board[i][col + j])
        return 1;
  }
  return 0;
}
int solve(int board[][8], int row) {
  if (row == 8) {
    print_board(board);
    return 0;
  }
  int i;
  for (i = 0; i < 8; i++) {
    if (conflict(board, row, i) == 0) {
      board[row][i] = 1;
      solve(board, row + 1);
      board[row][i] = 0;
    }
  }
}

int main() {
  int board[8][8];
  int i, j;
  for (i = 0; i < 8; i++)
    for (j = 0; j < 8; j++)
      board[i][j] = 0;
  solve(board, 0);
  return 0;
}
)";
