
<!--
    To generate the readme, run:

    docker run -ti --rm -v DOLLARSIGN(pwd):/test/usr maltegruber/readme-tex:1.0.0

    see: https://github.com/MalteGruber/readme-tex

-->



# 'the chase' implementation

This contains implementations of some papers on the chase. I'll attempt to summarize the what and why of the chase below.

## Background - relational algebra

Connected with relational databases that you might be more familiar with is the concept of *relational algebra*. What we called tables are now called *relations*, which we can think of as sets of tuples of some fixed length. Importantly we can ask whether a given tuple $t=(a,b, c, ...)$ is an element of a given relation $R$, i.e. is it the case that $t \in R$? We can represent a tuple as being an element of $R$ in particular by a subscript $(12,\ ``cat",\ x)_R$. We can also isolate particular attributes of a tuple by projection, which we could use subscripts for again: e.g. if the previous tuple was $t$ then $t_2 = ``cat''$.


$$\begin{tabular}{ |c|c| } \hline
 \multicolumn{2}{ |c| }{Likes}   \\ \hline
 \textbf{Liker}  & \textbf{Likee}\\ \hline
 Matt            & John          \\ \hline
 John            & John          \\ \hline
 John            & Mary          \\ \hline
\end{tabular}$$


$$\begin{tabular}{ |c|c| } \hline
 \multicolumn{2}{ |c| }{Manage}   \\ \hline
 \textbf{Liker}  & \textbf{Likee}\\ \hline
 Matt            & John          \\ \hline
 John            & John          \\ \hline
 Mary            & John          \\ \hline
\end{tabular}$$


We can use first-order logic to express constraints. We can express that "`Name` is a *primary key* of `Manage`" by $\forall x, y \in Manage: x_{Name}=y_{Name} \rightarrow x=y$. We can express that "`likee` is a *foreign key* $Likes \rightarrow Manage$" by writing $\forall liker, likee: (liker, likee)_{Likes} \rightarrow \exists mgr: (likee, mgr)_{Manage}$

Now suppose we have a database $I$ and a set constraints $\Sigma$, we can ask whether $I$ satisfies $\Sigma$. If not, we can imagine there being the best or nearest-relative of $I$ that *does* satisfy $I$. The chase is an algorithm that finds this other database given $I$ and $\Sigma$. The remaining background material to cover concerns how define this notion of a "best" relative of $I$.

## Background - database homomorphisms

One interesting relationship betweeen databases is whether one is a subset of the other.

$$\begin{tabular}{ |c|c| } \hline
 \multicolumn{2}{ |c| }{R}   \\ \hline
 \textbf{1}  & \textbf{2}\\ \hline
 200            & 200          \\ \hline
\end{tabular}$$

$$\begin{tabular}{ |c|c| } \hline
 \multicolumn{2}{ |c| }{S} \\ \hline
 \textbf{1}  & \textbf{2}  \\ \hline
 7              & 8        \\ \hline
 200            & 200      \\ \hline
\end{tabular}$$


We'll consider single-relation database instances in this section to keep the examples small, which allows us to abuse notation and treat the relations as databases in their own right. In the above examples, we could say $R \subset S$.

Our example databases were fully specified, but in general databases in this formalism can have "labeled nulls" which behave like variables.


$$\begin{tabular}{ |c|c| } \hline
 \multicolumn{2}{ |c| }{S} \\ \hline
 \textbf{1}  & \textbf{2}  \\ \hline
 a            & a        \\ \hline
 b            & 200      \\ \hline
\end{tabular}$$

This new version of $S$ says there's a tuple that has two of the same unknown value, and another which has an unknown first value (not necessarily distinct from the first tuple) and $200$ for the second value.


The instance that the chase returns is *universal*.

## What

## Why

## References
1. Gösta Grahne and Adrian Onet. *Anatomy of the Chase*. Fundamenta Informaticae 157 (2018) 221–270
2.