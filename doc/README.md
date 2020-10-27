<!--
    To generate the readme, run:

    python -m readme2tex --output README.md .README.md
-->

# 'the chase' implementation

This contains implementations of some papers on the chase. I'll attempt to summarize the what and why of the chase below.

## Background

Connected with relational databases that you might be more familiar with is the concept of *relational algebra*. What we called tables are now called *relations*, which we can think of as sets of tuples of some fixed length. We can use first order logic to express constraints.

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

We can express that "`Name` is a primary key of `Manage` by $\forall x$. We can express that `likee` is a foreign key $Likes \rightarrow Manage$

$$a^2 = b^2$$

$a^2 = b^2$

$\begin{tabular}{ |p{3cm}||p{3cm}|p{3cm}|p{3cm}|  }
 \hline
 \multicolumn{4}{|c|}{Country List} \\
 \hline
 Country Name     or Area Name& ISO ALPHA 2 Code &ISO ALPHA 3 Code&ISO numeric Code\\
 \hline
 Afghanistan   & AF    &AFG&   004\\
 Aland Islands&   AX  & ALA   &248\\
 Albania &AL & ALB&  008\\
 Algeria    &DZ & DZA&  012\\
 American Samoa&   AS  & ASM&016\\
 Andorra& AD  & AND   &020\\
 Angola& AO  & AGO&024\\
 \hline
\end{tabular}$

## What

## Why

## References
1. Gösta Grahne and Adrian Onet. *Anatomy of the Chase*. Fundamenta Informaticae 157 (2018) 221–270
2.