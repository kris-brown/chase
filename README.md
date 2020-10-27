
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


![a^2 = b^2](doc/teximg/tex_img_0_ZYQDB.png)

![a^2 = b^2](doc/teximg/tex_img_1_4NDPO.png)



![\begin{tabular}{ |c|c| } \hline \multicolumn{2}{ |c| }{Likes}   \\ \hline \textbf{Liker}  & \textbf{Likee}\\ \hline Matt            & John          \\ \hline John            & John          \\ \hline John            & Mary          \\ \hline\end{tabular}](doc/teximg/tex_img_2_JFEBO.png)


![\begin{tabular}{ |c|c| } \hline \multicolumn{2}{ |c| }{Manage}   \\ \hline \textbf{Liker}  & \textbf{Likee}\\ \hline Matt            & John          \\ \hline John            & John          \\ \hline Mary            & John          \\ \hline\end{tabular}](doc/teximg/tex_img_3_L06KF.png)

We can use first order logic to express constraints. We can express that "`Name` is a primary key of `Manage` by ![\forall n, m_1, m_2: (n, m_1) \land (n, m_2) \rightarrow m_1=m_2](doc/teximg/tex_img_4_ZHV0L.png). We can express that `likee` is a foreign key ![Likes \rightarrow Manage](doc/teximg/tex_img_5_Y8134.png) by writing ![\forall r, e: (r, e) \rightarrow \exists m: (e, m)](doc/teximg/tex_img_6_842WB.png)

## What

## Why

## References
1. Gösta Grahne and Adrian Onet. *Anatomy of the Chase*. Fundamenta Informaticae 157 (2018) 221–270
2.