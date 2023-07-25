#ifndef MATRIX_H
#define MATRIX_H

#include <vector>
#include <utility>
#include <algorithm>

template<typename T>
class Matrix
{
    int _nrow;
    int _ncol;
    std::vector<T> _data;

public:
    Matrix() = delete;

    Matrix(int nrow, int ncol, const std::vector<T>& data) : _nrow{nrow}, _ncol{ncol}, _data{data} {}
    Matrix(int nrow, int ncol, std::vector<T>&& data) : _nrow{nrow}, _ncol{ncol}, _data{std::move(data)} {}
    Matrix(int nrow, int ncol, T val = T{}) : _nrow{nrow}, _ncol{ncol}, _data(nrow * ncol, val) {}

    const std::vector<T>& data() const { return _data; }
    int size() const { return _data.size(); }
    int nrow() const { return _nrow; }
    int ncol() const { return _ncol; }
    std::pair<int, int> dims() const { return {_nrow, _ncol}; }

    bool in_bound(int r, int c) const { return r >= 0 && c >= 0 && r < _nrow && c < _ncol; }
    T get_element(int r, int c) const { return _data[r * _ncol + c]; }
    T get_element_or(int r, int c, T val) const { return in_bound(r, c) ? get_element(r, c) : val; }
    void set_element(int r, int c, T val) { _data[r * _ncol + c] = val; }
    
    T max_element() const { return *std::max_element(_data.begin(), _data.end()); }
    T min_element() const { return *std::min_element(_data.begin(), _data.end()); }
    
    template<typename F>
    void for_each(F fn) const { std::for_each(_data.begin(), _data.end(), fn); }

    template<typename U>
    Matrix<U> as_type() const { return Matrix{_nrow, _ncol, std::vector<U>(_data.begin(), _data.end())}; }
};

// Vanilia 2d convolution
static Matrix<double> convolve(const Matrix<double>& matrix, const Matrix<double>& kernel)
{
    auto [mr, mc] = matrix.dims();
    auto [kr, kc] = kernel.dims();
    
    int hkr = kr / 2;
    int hkc = kc / 2;

    Matrix<double> new_matrix = Matrix<double>{mr, mc};

    for (int r = 0; r < mr; r++) {
        for (int c = 0; c < mc; c++) {
            int rs = r - hkr;
            int cs = c - hkc;
            double nme = 0;
            // we will not normalize. Use `map` to normalize stuffs
            for (int t = 0; t < kr; t++) {
                for (int u = 0; u < kc; u++) {
                    // calculate the index
                    double me = matrix.get_element_or(rs + t, cs + u, 0);
                    double ke = kernel.get_element(t, u);
                    nme += me * ke;
                }
            }
            // set the new value to the new matrix
            new_matrix.set_element(r, c, nme);
        }
    }
    return new_matrix;
}

template<typename T, typename R, typename F>
static Matrix<R> apply(const Matrix<T>& matrix, F fn)
{
    auto [nr, nc] = matrix.dims();
    Matrix<R> result{nr, nc};
    for (int r = 0; r < nr; r++) {
        for (int c = 0; c < nc; c++) {
            T me = matrix.get_element(r, c);
            result.set_element(r, c, fn(me));
        }
    }
    return result;
}

static Matrix<double> scale(const Matrix<double>& matrix, double max_val)
{
    double max_elt = matrix.max_element();
    return apply<double, double>(matrix, [max_val, max_elt] (double d) { return d / max_elt * max_val; });
}

static Matrix<double> guassian_filter(const Matrix<double>& matrix)
{
    const Matrix<double> filter{5, 5, {
        0.013, 0.025, 0.031, 0.025, 0.013,
        0.025, 0.057, 0.075, 0.057, 0.025, 
        0.031, 0.075, 0.094, 0.075, 0.031,
        0.025, 0.057, 0.075, 0.057, 0.025,
        0.013, 0.025, 0.031, 0.025, 0.013,
    }};
    return convolve(matrix, filter);
}

#endif // MATRIX_H
