#include <Rcpp.h>
using namespace Rcpp;
                    
                    // Map probs in [0,1] to Type-1 indices 0..n-1 (inverse ECDF).
                    // idx = ceil(p * n) - 1, clamped to [0, n-1]
                    static inline R_xlen_t prob_to_idx1(double p, R_xlen_t n) {
                      if (p <= 0.0) return 0;
                      if (p >= 1.0) return n - 1;
                      double h = std::ceil(p * (double)n);
                      R_xlen_t idx = (R_xlen_t)h - 1;
                      if (idx < 0) idx = 0;
                      if (idx >= n) idx = n - 1;
                      return idx;
                    }
                    
                    // Partition-aware multi-select using nth_element on shrinking subranges.
                    // Expected O(n log m) where m = number of unique order stats.
                    static void select_many(NumericVector& y,
                                            R_xlen_t lo, R_xlen_t hi,           // active range [lo, hi)
                                            std::vector<R_xlen_t>& ks,          // target indices (global, sorted, unique)
                                            R_xlen_t k_lo, R_xlen_t k_hi,       // slice of ks: [k_lo, k_hi)
                                            std::vector<double>& out_values)    // results by global k
                    {
                      if (k_lo >= k_hi || hi - lo <= 0) return;
                      
                      // Choose the middle target for balance.
                      R_xlen_t mid = (k_lo + k_hi) / 2;
                      R_xlen_t k   = ks[mid];
                      
                      // Place the k-th smallest (global index) into position k via nth_element,
                      // but we must translate "k" to the current subrange boundaries.
                      // We know lo <= k < hi in a correct call.
                      // Apply nth_element over [lo, hi) with nth = y.begin() + k
                      std::nth_element(y.begin() + lo, y.begin() + k, y.begin() + hi);
                      out_values[k] = y[k]; // record the exact value at rank k
                      
                      // Left subset: targets < k  in [lo, k)
                      if (k_lo < mid && k > lo) {
                        select_many(y, lo, k, ks, k_lo, mid, out_values);
                      }
                      // Right subset: targets > k in [k+1, hi)
                      if (mid + 1 < k_hi && k + 1 < hi) {
                        select_many(y, k + 1, hi, ks, mid + 1, k_hi, out_values);
                      }
                    }
                    
                    // [[Rcpp::export]]
                    NumericVector quantile1_large_vec(NumericVector x, NumericVector probs) {
                      const R_xlen_t n  = x.size();
                      const R_xlen_t np = probs.size();
                      
                      if (np == 0) return NumericVector(0);
                      if (n == 0) return NumericVector(np, NA_REAL);
                      
                      // Clone because we’ll permute in-place
                      NumericVector y = Rcpp::clone(x);
                      
                      // Map probs -> target order indices (Type-1), collect & sort unique ks
                      std::vector<R_xlen_t> k_all(np);
                      for (R_xlen_t i = 0; i < np; ++i) {
                        double p = probs[i];
                        // treat NA/NaN as NA in output
                        if (!R_finite(p)) { k_all[i] = -1; continue; }
                        if (p < 0.0) p = 0.0;
                        if (p > 1.0) p = 1.0;
                        k_all[i] = prob_to_idx1(p, n);
                      }
                      
                      // Extract unique ks (>=0), sorted
                      std::vector<R_xlen_t> ks = k_all;
                      ks.erase(std::remove_if(ks.begin(), ks.end(),
                                              [](R_xlen_t v){ return v < 0; }),
                                                                         ks.end());
                      std::sort(ks.begin(), ks.end());
                      ks.erase(std::unique(ks.begin(), ks.end()), ks.end());
                      
                      // If no valid probs, return all NAs
                      NumericVector out(np, NA_REAL);
                      if (ks.empty()) return out;
                      
                      // Bucket for answers addressed by global rank k
                      std::vector<double> by_rank(n, NA_REAL);
                      
                      // Multi-select in-place
                      select_many(y, 0, n, ks, 0, (R_xlen_t)ks.size(), by_rank);
                      
                      // Scatter results back to requested order (including duplicates)
                      for (R_xlen_t i = 0; i < np; ++i) {
                        R_xlen_t k = k_all[i];
                        if (k >= 0) out[i] = by_rank[k];
                      }
                      return out;
                    }
