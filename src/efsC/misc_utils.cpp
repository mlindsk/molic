/*****************************************
 * In:
 * - x: Vector of strings
 * Out: All pairwise combinations (i, j) for i != j
 ***************************************/
Rcpp::CharacterMatrix pairwise_comb(VS x) {
  const int N = x.size();
  const int  n_complete = N * (N - 1) / 2;
  Rcpp::CharacterMatrix A(n_complete, 2);
  int row = 0;
  for( int i = 0; i < N; ++i ) {
    for( int j = i+1; j < N; ++j ) {
      A(row, 0) = x[i];
      A(row, 1) = x[j];
      row++;
    }
  }
  return A;
}

