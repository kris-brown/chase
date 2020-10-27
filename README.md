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

We can express that "`Name` is a primary key of `Manage` by ![\forall x](doc/teximg/tex_img_0_WKAN7.png). We can express that `likee` is a foreign key ![Likes \rightarrow Manage](doc/teximg/tex_img_1_2UQJ9.png)

![a^2 = b^2](doc/teximg/tex_img_2_9XE3F.png)

![a^2 = b^2](doc/teximg/tex_img_3_QJ98W.png)

![\begin{tabular}{ |p{3cm}||p{3cm}|p{3cm}|p{3cm}|  } \hline \multicolumn{4}{|c|}{Country List} \\ \hline Country Name     or Area Name& ISO ALPHA 2 Code &ISO ALPHA 3 Code&ISO numeric Code\\ \hline Afghanistan   & AF    &AFG&   004\\ Aland Islands&   AX  & ALA   &248\\ Albania &AL & ALB&  008\\ Algeria    &DZ & DZA&  012\\ American Samoa&   AS  & ASM&016\\ Andorra& AD  & AND   &020\\ Angola& AO  & AGO&024\\ \hline\end{tabular}](doc/teximg/tex_img_4_ZHI4Z.png)

## What

## Why

## References
1. Gösta Grahne and Adrian Onet. *Anatomy of the Chase*. Fundamenta Informaticae 157 (2018) 221–270
2.