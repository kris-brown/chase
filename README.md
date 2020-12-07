
<!--
    To generate the readme, run:

    docker run -ti --rm -v /Users/ksb/chase:/test/usr maltegruber/readme-tex:1.0.0

    see: https://github.com/MalteGruber/readme-tex

-->



# 'The chase' implementation

This contains implementations of some papers on the chase. I'll attempt to summarize the what and why of the chase below.

## Background - relational algebra

Connected with relational databases that you might be more familiar with is the concept of *relational algebra*. What we called tables are now called *relations*, which we can think of as sets of tuples of some fixed length. Importantly we can ask whether a given tuple ![t=(a,b, c, ...)](doc/teximg/tex_img_0_I431C.png) is an element of a given relation ![R](doc/teximg/tex_img_1_LCPZB.png), i.e. is it the case that ![t \in R](doc/teximg/tex_img_2_91HWH.png)? We can isolate particular attributes of a tuple by projection, which we could use subscripts for: e.g. if the previous tuple was ![t](doc/teximg/tex_img_3_Y6URT.png), then ![t_2 = b](doc/teximg/tex_img_4_Q06YJ.png). It's easiest to visualize the relations as tables:


![\begin{tabular}{ |c|c| } \hline \multicolumn{2}{ |c| }{Likes}   \\ \hline \textbf{Liker}  & \textbf{Likee}\\ \hline Matt            & John          \\ \hline John            & John          \\ \hline John            & Mary          \\ \hline\end{tabular}](doc/teximg/tex_img_5_JJROM.png)


![\begin{tabular}{ |c|c| } \hline \multicolumn{2}{ |c| }{Manage}   \\ \hline \textbf{Employee}  & \textbf{Manager}\\ \hline Matt            & John          \\ \hline John            & John          \\ \hline Mary            & John          \\ \hline\end{tabular}](doc/teximg/tex_img_6_6HAHZ.png)


We can use [first-order logic](https://www.javatpoint.com/first-order-logic-in-artificial-intelligence) to express constraints. We can express that "`Employee` is a *primary key* of `Manage`" by ![\forall x, y \in Manage: x_{Employee}=y_{Employee} \Rightarrow x=y](doc/teximg/tex_img_7_D4FXG.png). We can express that "`Likee` is a *foreign key* ![Likes \rightarrow Manage](doc/teximg/tex_img_8_UNBRA.png)" by writing ![\forall x \in Likes: \exists mgr: (x_{Likee},\ mgr) \in {Manage}](doc/teximg/tex_img_9_28RVE.png)

Now suppose we have a database ![I](doc/teximg/tex_img_10_N71E0.png) and a set constraints ![\Sigma](doc/teximg/tex_img_11_NY9KT.png), we can ask whether ![I](doc/teximg/tex_img_12_47RCP.png) satisfies ![\Sigma](doc/teximg/tex_img_13_FPNMS.png). If not, we can imagine there being the best or nearest-relative of ![I](doc/teximg/tex_img_14_UH206.png) that *does* satisfy ![\Sigma](doc/teximg/tex_img_15_FHSQZ.png). **The chase** is an algorithm that finds this other database, given ![I](doc/teximg/tex_img_16_J49GX.png) and ![\Sigma](doc/teximg/tex_img_17_I61OP.png). The remaining background material to cover concerns how to define this notion of a "best" relative of ![I](doc/teximg/tex_img_18_JN93Y.png).

## Background - database homomorphisms

One interesting relationship between databases is whether one is a subset of the other.

![\begin{tabular}{ |c|c| } \hline \multicolumn{2}{ |c| }{R}   \\ \hline \textbf{1}  & \textbf{2}\\ \hline 200            & 200          \\ \hline\end{tabular}](doc/teximg/tex_img_19_POHL6.png)

![\begin{tabular}{ |c|c| } \hline \multicolumn{2}{ |c| }{Q} \\ \hline \textbf{1}  & \textbf{2}  \\ \hline 7              & 8        \\ \hline 200            & 200      \\ \hline\end{tabular}](doc/teximg/tex_img_20_SRJ63.png)


We'll consider single-relation database instances in this section to keep the examples small, which allows us to abuse notation and treat the relations as databases in their own right. In the above examples, we could say ![R \subset Q](doc/teximg/tex_img_21_DWZ17.png) to mean one database is a subset of the other.

Although all example databases above have been fully specified, generally databases in this formalism can have "labeled nulls" which behave like variables.


![\begin{tabular}{ |c|c| } \hline \multicolumn{2}{ |c| }{S} \\ \hline \textbf{1}  & \textbf{2}  \\ \hline a            & a        \\ \hline b            & 200      \\ \hline\end{tabular}](doc/teximg/tex_img_22_TTXNU.png)

This new instance ![S](doc/teximg/tex_img_23_EL46K.png) says there's a tuple that has two of the same unknown value, and another which has an unknown first value and ![200](doc/teximg/tex_img_24_TXBXV.png) for the second value. We need to refine our notion of ![\subset](doc/teximg/tex_img_25_V7KI3.png) in light of the possibility of labeled nulls. The relevant relation now is called a *homomorphism* (![A \rightarrow B](doc/teximg/tex_img_26_8O7FY.png)) which consists in an assignment to the labeled nulls in ![A](doc/teximg/tex_img_27_TH7NL.png) such that, when substituting ![A](doc/teximg/tex_img_28_ACP50.png) with this assignment, we get a database instance that is literally a subset of ![B](doc/teximg/tex_img_29_L13XB.png). For example, the assignment ![\{a \mapsto 200, b \mapsto 200\}](doc/teximg/tex_img_30_QZ3T9.png) is a homomorphism ![S \rightarrow R](doc/teximg/tex_img_31_WW6W5.png). There is no homomorphism ![R \rightarrow S](doc/teximg/tex_img_32_3HN55.png). (![R \rightarrow Q](doc/teximg/tex_img_33_NFBX4.png) is a homomorphism trivially, no substitution needed; for any ![A](doc/teximg/tex_img_34_JRF2Q.png) we have ![A \rightarrow A](doc/teximg/tex_img_35_H0ZWF.png), given we can make an identity substitution).

Even with merely a simple binary relation like ![\rightarrow](doc/teximg/tex_img_36_9Z5XJ.png), we can derive a notion of the 'closest' related ![U](doc/teximg/tex_img_37_WX2IG.png) for some given ![I](doc/teximg/tex_img_38_QYHS9.png). Fix a given ![\Sigma](doc/teximg/tex_img_39_CAQDA.png) of constraints and imagine the set of all possible instances which both satisfy ![\Sigma](doc/teximg/tex_img_40_NU7BV.png) and are related to ![I](doc/teximg/tex_img_41_2KTTX.png). Among this set, it may be the case that there exists a ![U](doc/teximg/tex_img_42_05J2A.png) such that ![I \rightarrow U \rightarrow X](doc/teximg/tex_img_43_N0CIJ.png) for all ![X](doc/teximg/tex_img_44_4G19U.png) instances in this set, i.e. *any* homomorphism can be factored through a homomorphism with ![U](doc/teximg/tex_img_45_UDLTH.png).

(Analgously, consider the relation ![\le](doc/teximg/tex_img_46_MVKP5.png) for integers, let ![I=12](doc/teximg/tex_img_47_V0C8W.png) and ![\Sigma = \{> 14, < 20\}](doc/teximg/tex_img_48_IQ751.png) ... we want something that is 'related' to ![I](doc/teximg/tex_img_49_J5NE4.png), so we are looking for an ![i](doc/teximg/tex_img_50_LA5T6.png) such that ![12 \le i](doc/teximg/tex_img_51_E32LX.png) which also satisfies the constraints. There exist 5 possible candidates (![15](doc/teximg/tex_img_52_YRFX0.png) through ![19](doc/teximg/tex_img_53_QXND6.png) inclusive) and it happens that for any candidate ![X](doc/teximg/tex_img_54_DC0QQ.png) we can write ![I \le U \le X](doc/teximg/tex_img_55_CUW3I.png) with the value of ![U=15](doc/teximg/tex_img_56_G6JIC.png). Thus, ![15](doc/teximg/tex_img_57_H5KZA.png) is the 'closest' integer to ![12](doc/teximg/tex_img_58_AO86M.png) that satisfies the constraints.)

The database instance ![U](doc/teximg/tex_img_59_IAXZR.png) that satisfies this property is called *universal* (with respect to ![I,\  \Sigma](doc/teximg/tex_img_60_VY1I8.png)), and this is the instance that the chase returns, if it exists.

## Why
Reading about what the chase is and how it works will be more interesting with context as to why finding a universal database instance is of interest.
- Query Optimization
- Constraint Implication
- Universal Solutions / Data Exchange
- Certain Answers

## What
The simplest variation of the chase considers each constraint as a potential trigger which can be *fired* to produce a new instance that satisfies that constraint. We then iteratively repeat this process until there exist no active triggers (i.e. produce a sequence of instances until we arrive at one which satisfies the constraints).

The literature is generally concerned with two types of constraints.
- Tuple-generating dependencies: *if* there exist some tuples satisfying such-and-such conditions, *then* there must exist these other tuples satisfying such-and-such conditions. To *fire* this constraint is to add the tuples in the consequent clause.
- Equality-generating dependencies: *if* there exist some tuples (which involve two variables, say ![x](doc/teximg/tex_img_61_OAJI6.png) and ![y](doc/teximg/tex_img_62_2PVFH.png)) satisfying such-and-such conditions, *then* ![x=y](doc/teximg/tex_img_63_IQBV6.png). To *fire* this constraint is to take all instances of ![y](doc/teximg/tex_img_64_OZ5LZ.png) and replace them with ![x](doc/teximg/tex_img_65_O14WH.png) (or vice-versa). If both ![x](doc/teximg/tex_img_66_HPTZG.png) and ![y](doc/teximg/tex_img_67_GL6B0.png) match with constants which have different values, then the chase fails.
- In both cases, there is a clear antecedent condition to check to see if the trigger is *active*.

It's possible for the sequence of instances generated by firing constraints to never terminate. For some simple chase implementations, this doesn't imply that a universal solution doesn't exist. However, more sophisticated implementations (such as the *core* chase) are complete, meaning they terminate iff there exists a solution.


## Functionality
This repo has implementations in different languages.
| Language      | Python | Haskell |
| ----------- | ----------- |---- |
| Read relations / database instances from CSV      | <ul><li>- [x]</li><li>      |  <ul><li>- []</li><li> |
| Basic chase algorithm  | <ul><li>- []</li><li>        |<ul><li>- []</li><li>|



## References
1. Gösta Grahne and Adrian Onet. *Anatomy of the Chase*. Fundamenta Informaticae. **157** (2018) *221–270*.
2. Alin Deustch, Alan Nash, and Jeff Remmel. *The chase revisited*. PODS (2008).