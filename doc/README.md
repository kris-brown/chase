
<!--
    To generate the readme, run:

    docker run -ti --rm -v DOLLARSIGN(pwd):/test/usr maltegruber/readme-tex:1.0.0

    see: https://github.com/MalteGruber/readme-tex

-->



# 'the chase' implementation

This contains implementations of some papers on the chase. I'll attempt to summarize the what and why of the chase below.

## Background

Connected with relational databases that you might be more familiar with is the concept of *relational algebra*. What we called tables are now called *relations*, which we can think of as sets of tuples of some fixed length.

### Likes
| liker| likee |
|------|--------|
| Matt | John   |
| John  |  John |
| John | Mary   |
### Manage
|  name| manager |
|------|--------|
| Matt | John   |
| John |  John |
| Mary | John   |


$a^2 = b^2$

$a^2 = b^2$



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

We can use first order logic to express constraints. We can express that "`Name` is a primary key of `Manage` by $\forall n, m_1, m_2: (n, m_1) \land (n, m_2) \rightarrow m_1=m_2$. We can express that `likee` is a foreign key $Likes \rightarrow Manage$ by writing $\forall r, e: (r, e) \rightarrow \exists m: (e, m)$

## What

## Why

## References
1. Gösta Grahne and Adrian Onet. *Anatomy of the Chase*. Fundamenta Informaticae 157 (2018) 221–270
2.