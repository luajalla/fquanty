**fquanty** is a self-contained collection of basic stats and performance functions, which can be extremely useful for modelling.

Currently the library contains the following functions:  

	- Statistics  
		- variance
		- standard deviation
		- covariance
		- correlation
		- covariance matrix
		- correlation matrix
		- downside deviation
	- Time Value of Money
		- fv
		- pv
		- pmt
		- fv of annuity
		- pv of annuity
		- pv of perpetuity
		- Effective Annual Rates
		- nper
		- rate
    - Performance.Returns
		- simple/cc returns
		- rolling returns
		- annualization
		- active return
		- Value-at-Risk
    - Performance.Portfolio
		- variance
		- standard deviation
		- expected return
		- excess returns
		- global minimum variance portfolio
		- beta
		- sharpe ratio
		- information ratio
		- kelly ratio
		- treynor ratio
		- sortino ratio
		- Value-at-Risk
	- Performance.Bootstrapping
		- randomize
		- bootstrap
		- bootstrapMean
		- bootstrapStd
		- boostrapVaR
    - Data Providers
		- fetch Yahoo quotes
		- fetch Google quotes
	- Gamma
		- ln of the Gamma function
		- incomplete Gamma function
	- Uniform Distribution
		- mean
		- variance
		- density
		- distribution
		- quantile
		- generating random values
	- Normal Distribution
		- quantile
    - Solver
		- newton
		- bisection
		- brent
	- Matrix Inv
		- LU decomposition
		- Inverse matrix

The notation for _TVM_ functions is similar to Excel ones for convinience.

The library is inspired mostly by R’s [financial packages](http://cran.r-project.org/web/views/Finance.html), in particular [Performance Analytics](http://cran.r-project.org/web/packages/PerformanceAnalytics/index.html) and [quantmod](http://cran.r-project.org/web/packages/quantmod/index.html).

Gamma-related functions are based on implementations [here](http://people.sc.fsu.edu/~jburkardt/c_src/c_src.html): 
[the incomplete gamma function](// http://people.sc.fsu.edu/~jburkardt/c_src/asa147/asa147.html) and [logarithm of gamma function](http://people.sc.fsu.edu/~jburkardt/c_src/toms291/toms291.html). 
Root-finding function is a combination of Newton method and [Brent’s zero finder](http://www.netlib.org/c/brent.shar)
     

_Note, that this library doesn’t pretend to be the fastest or the most precise solution, as stated above it’s purpose is simple modelling and minimizing the dependencies (the only external reference is FSharp.PowerPack.dll)_