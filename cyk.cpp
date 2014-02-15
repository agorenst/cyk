// by Aaron Gorenstein

#include <map>
#include <list>
#include <set>
#include <iostream>
#include <sstream>
#include <iterator>
#include <fstream>
#include <vector>

using namespace std;

// this is a clever way of tokenizing strings
// delimited by whitespace in C++, using the code
// by "KeithB" at http://stackoverflow.com/questions/53849/how-do-i-tokenize-a-string-in-c
// (note: I was neither the answerer nor the asker)
//
// So the CFG file format is : Nonterminal Production Production
pair<string, list<string>> parse_prod(string s) {
    stringstream str(s);
    string lhs;
    str >> lhs;
    istream_iterator<string> it(str), end;
    list<string> rhs(it,end);
    return pair<string, list<string>>(lhs,rhs);
}

// handy debugging routine.
string slice(string s, int i, int j) {
    if (j < i) {
        return "";
    }
    return s.substr(i,j-i+1);
}

// handy debugging output things;
ostream& operator<<(ostream& o, set<string> s) {
    o << "{ ";
    for (auto it = s.begin(); it != s.end(); ++it) {
        o << *it << " ";
    }
    o << "}";
    return o;
}



// can't believe this isn't in algorithms: I guess because it is a pass-by-value fn for iterators,
// may behave oddly.
template<class iter>
unsigned length(iter begin, iter end) {
    unsigned n = 0;
    for (; begin != end; ++begin) {
        ++n;
    }
    return n;
}

// can be made more efficient, space wise, etc.
// Allows for opportunity to do really clever memory-management stuff,
// if I replace this value with a custom-made class.
template<class S>
class cyktable {
    private:
    vector<vector<set<S>>> M;
    public:
    cyktable(unsigned n): M(n) {
        for (unsigned i = 0; i < n; ++i) { M[i] = vector<set<S>>(n); }
    }
    vector<set<S>>& operator[](int i) { return M[i]; }
    unsigned size() const { return M.size(); }
};

// quick and easy way of printing out the dyn prog table for
template<class S>
ostream& operator<<(ostream& o, cyktable<S>& m) {
    for (unsigned i = 0; i < m.size(); ++i) {
        for (unsigned j = 0; j < m[i].size(); ++j) {
            o << i << ", " << j << " : " << m[i][j] << endl;
        }
    }
    return o;
}

// this is to make a string "abcd" into ["a", "b", ...].
// Note that iterating over the string normally treats
// each character as a char, not a string.
list<string> make_unit_strings(string s) {
    list<string> l;
    for (auto it = s.begin(); it != s.end(); ++it) {
        string r = "";
        r += *it;
        l.push_back(r);
    }
    return l;
}

/* This is the start of the "real" algorithms. Here we have the design of our CFG class
 * and of course the CYK algorithm.
 * These can both be extended: there are many fun extensions I can think of.
 * EG, the CFG class trusts that you're making a CNF grammar: it could implement the
 * algorithm to *make* itself a CNF, allowing for general grammar input.
 *
 * The CYK algorithm could be extended to output the parse tree if one exists.
 * I believe I've made it type-generic enough, though there are a few things
 * still to be addressed.
 */


// This is really just a two-way multimap
// that has particular search methods.
template<class S>
class cfg {
    multimap<S, list<S>> productions;
    multimap<list<S>, S> reverse_prod;

    public:
    const S start_symb;
    cfg(): start_symb("S") {}

    void insert(pair<S, list<S>> p) {
        productions.insert(p);
        reverse_prod.insert(pair<list<S>,S>(p.second, p.first));
    }

    // returns all symbols that generate the provided RHS.
    set<S> generators(initializer_list<S> input) {
        list<S> ind(input.begin(), input.end());
        auto range = reverse_prod.equal_range(ind);
        set<S> s;
        for (auto it = range.first; it != range.second; ++it) {
            s.insert(it->second);
        }
        return s;
    }

};


// The CYK algorithm.
// 
// todo: output parse tree?
// :concerned about iter qualities: can we trust we can iterate
// over the sequence multiple times? There's some iterator tag for that.
template<class S, class iter>
bool cyk(cfg<S> g, unsigned n, iter seq_begin, iter seq_end) {
    cyktable<S> M(n);

    for (unsigned i = 0; i < n && seq_begin != seq_end; ++i, ++seq_begin) {
        M[i][i] = g.generators({*seq_begin});
    }
    
    for (unsigned l = 1; l < n; ++l) { // for each length
        for (unsigned r = 0; r < n-l; ++r) { // for each start
            // the string we're considering
            for (unsigned t = 0; t < l; ++t) { // for each split-point
                // what generates the two halves of this split?
                auto lhs = M[r][r+t];
                auto rhs = M[r+t+1][r+l];
                for (auto it = lhs.begin(); it != lhs.end(); ++it) {
                    for (auto jt = rhs.begin(); jt != rhs.end(); ++jt) {
                        // "gens" is the set of all nonterminals generating itjt.
                        auto gens = g.generators({*it,*jt});
                        M[r][r+l].insert(gens.begin(),gens.end());
                    }
                }
            }
        }
    }
    return M[0][n-1].find(g.start_symb) != M[0][n-1].end();
}


int main(int argc, char* argv[]) {
    fstream f;
    f.open(argv[1]);
    string line;
    cfg<string> g;
    while(getline(f,line)) {
        pair<string, list<string>> prod = parse_prod(line);
        g.insert(prod);
    }
    string s(argv[2]);

    // the input is just one ``word'', but we want to treat each character
    // as a token, and in our algorithm tokens are strings.
    list<string> l = make_unit_strings(s);
    cout << cyk(g, l.size(), l.begin(), l.end()) << endl;
}

// by Aaron Gorenstein
