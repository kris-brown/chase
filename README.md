
<!--
    To generate the readme, run:

    docker run -ti --rm -v DOLLARSIGN(pwd):/test/usr maltegruber/readme-tex:1.0.0

    see: https://github.com/MalteGruber/readme-tex

-->



# 'The chase' implementation

This contains implementations of some papers on the chase. I'll attempt to summarize the what and why of the chase below.

## Background - relational algebra

Connected with relational databases that you might be more familiar with is the concept of *relational algebra*. What we called tables are now called *relations*, which we can think of as sets of tuples of some fixed length. Importantly we can ask whether a given tuple ![t=(a,b, c, ...)](doc/teximg/tex_img_0_ONJPM.png) is an element of a given relation ![R](doc/teximg/tex_img_1_YBIF0.png), i.e. is it the case that ![t \in R](doc/teximg/tex_img_2_SXTHH.png)? We can isolate particular attributes of a tuple by projection, which we could use subscripts for: e.g. if the previous tuple was ![t](doc/teximg/tex_img_3_SWVJN.png), then ![t_2 = b](doc/teximg/tex_img_4_X6QQ1.png). It's easiest to visualize the relations as tables:


![\begin{tabular}{ |c|c| } \hline \multicolumn{2}{ |c| }{Likes}   \\ \hline \textbf{Liker}  & \textbf{Likee}\\ \hline Matt            & John          \\ \hline John            & John          \\ \hline John            & Mary          \\ \hline\end{tabular}](doc/teximg/tex_img_5_X3TJY.png)


![\begin{tabular}{ |c|c| } \hline \multicolumn{2}{ |c| }{Manage}   \\ \hline \textbf{Employee}  & \textbf{Manager}\\ \hline Matt            & John          \\ \hline John            & John          \\ \hline Mary            & John          \\ \hline\end{tabular}](doc/teximg/tex_img_6_VTNGP.png)


We can use [first-order logic](https://www.javatpoint.com/first-order-logic-in-artificial-intelligence) to express constraints. We can express that "`Employee` is a *primary key* of `Manage`" by ![\forall x, y \in Manage: x_{Employee}=y_{Employee} \Rightarrow x=y](doc/teximg/tex_img_7_91VYQ.png). We can express that "`Likee` is a *foreign key* ![Likes \rightarrow Manage](doc/teximg/tex_img_8_H8538.png)" by writing ![\forall x \in Likes: \exists mgr: (x_{Likee},\ mgr) \in {Manage}](doc/teximg/tex_img_9_H2YO5.png)

Now suppose we have a database ![I](doc/teximg/tex_img_10_KUDPC.png) and a set constraints ![\Sigma](doc/teximg/tex_img_11_IV7VZ.png), we can ask whether ![I](doc/teximg/tex_img_12_LBXQN.png) satisfies ![\Sigma](doc/teximg/tex_img_13_0K3UL.png). If not, we can imagine there being the best or nearest-relative of ![I](doc/teximg/tex_img_14_6AS9J.png) that *does* satisfy ![\Sigma](doc/teximg/tex_img_15_DRDN4.png). **The chase** is an algorithm that finds this other database, given ![I](doc/teximg/tex_img_16_L0ZZU.png) and ![\Sigma](doc/teximg/tex_img_17_ZT9YV.png). The remaining background material to cover concerns how to define this notion of a "best" relative of ![I](doc/teximg/tex_img_18_S7OFD.png).

## Background - database homomorphisms

One interesting relationship between databases is whether one is a subset of the other.

![\begin{tabular}{ |c|c| } \hline \multicolumn{2}{ |c| }{R}   \\ \hline \textbf{1}  & \textbf{2}\\ \hline 200            & 200          \\ \hline\end{tabular}](doc/teximg/tex_img_19_NG9BZ.png)

![\begin{tabular}{ |c|c| } \hline \multicolumn{2}{ |c| }{Q} \\ \hline \textbf{1}  & \textbf{2}  \\ \hline 7              & 8        \\ \hline 200            & 200      \\ \hline\end{tabular}](doc/teximg/tex_img_20_GV0B7.png)


We'll consider single-relation database instances in this section to keep the examples small, which allows us to abuse notation and treat the relations as databases in their own right. In the above examples, we could say ![R \subset Q](doc/teximg/tex_img_21_IZ93V.png) to mean one database is a subset of the other.

Although all example databases above have been fully specified, generally databases in this formalism can have "labeled nulls" which behave like variables.


![\begin{tabular}{ |c|c| } \hline \multicolumn{2}{ |c| }{S} \\ \hline \textbf{1}  & \textbf{2}  \\ \hline a            & a        \\ \hline b            & 200      \\ \hline\end{tabular}](doc/teximg/tex_img_22_ZXAJE.png)

This new instance ![S](doc/teximg/tex_img_23_XF1EH.png) says there's a tuple that has two of the same unknown value, and another which has an unknown first value and ![200](doc/teximg/tex_img_24_94M1C.png) for the second value. We need to refine our notion of ![\subset](doc/teximg/tex_img_25_S4BL0.png) in light of the possibility of labeled nulls. The relevant relation now is called a *homomorphism* (![A \rightarrow B](doc/teximg/tex_img_26_NWBRJ.png)) which consists in an assignment to the labeled nulls in ![A](doc/teximg/tex_img_27_U5X6M.png) such that, when substituting ![A](doc/teximg/tex_img_28_QSLHY.png) with this assignment, we get a database instance that is literally a subset of ![B](doc/teximg/tex_img_29_EJ994.png). For example, the assignment ![\{a \mapsto 200, b \mapsto 200\}](doc/teximg/tex_img_30_P9KSP.png) is a homomorphism ![S \rightarrow R](doc/teximg/tex_img_31_YKMDS.png). There is no homomorphism ![R \rightarrow S](doc/teximg/tex_img_32_S3G8P.png). (![R \rightarrow Q](doc/teximg/tex_img_33_XBQEJ.png) is a homomorphism trivially, no substitution needed; for any ![A](doc/teximg/tex_img_34_W5BF6.png) we have ![A \rightarrow A](doc/teximg/tex_img_35_J3J89.png), given we can make an identity substitution).

Even with merely a simple binary relation like ![\rightarrow](doc/teximg/tex_img_36_OLYUX.png), we can derive a notion of the 'closest' related ![U](doc/teximg/tex_img_37_48DOT.png) for some given ![I](doc/teximg/tex_img_38_DTQOH.png). Fix a given ![\Sigma](doc/teximg/tex_img_39_PSFP3.png) of constraints and imagine the set of all possible instances which both satisfy ![\Sigma](doc/teximg/tex_img_40_3FPE2.png) and are related to ![I](doc/teximg/tex_img_41_XPY3K.png). Among this set, it may be the case that there exists a ![U](doc/teximg/tex_img_42_INXFM.png) such that ![I \rightarrow U \rightarrow X](doc/teximg/tex_img_43_ZMINW.png) for all ![X](doc/teximg/tex_img_44_029HF.png) instances in this set, i.e. *any* homomorphism can be factored through a homomorphism with ![U](doc/teximg/tex_img_45_SCZWS.png).

(Analgously, consider the relation ![\le](doc/teximg/tex_img_46_3JXHO.png) for integers, let ![I=12](doc/teximg/tex_img_47_VRR6T.png) and ![\Sigma = \{> 14, < 20\}](doc/teximg/tex_img_48_31QQC.png) ... we want something that is 'related' to ![I](doc/teximg/tex_img_49_4O3GS.png), so we are looking for an ![i](doc/teximg/tex_img_50_HXXL5.png) such that ![12 \le i](doc/teximg/tex_img_51_1ZFGI.png) which also satisfies the constraints. There exist 5 possible candidates (![15](doc/teximg/tex_img_52_KUUP9.png) through ![19](doc/teximg/tex_img_53_MDFXA.png) inclusive) and it happens that for any candidate ![X](doc/teximg/tex_img_54_DTDWC.png) we can write ![I \le U \le X](doc/teximg/tex_img_55_CSFAX.png) with the value of ![U=15](doc/teximg/tex_img_56_3OG3I.png). Thus, ![15](doc/teximg/tex_img_57_TLIZ6.png) is the 'closest' integer to ![12](doc/teximg/tex_img_58_F9A7G.png) that satisfies the constraints.)

The database instance ![U](doc/teximg/tex_img_59_IF4ZU.png) that satisfies this property is called *universal* (with respect to ![I,\  \Sigma](doc/teximg/tex_img_60_X96BW.png)), and this is the instance that the chase returns, if it exists.

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
- Equality-generating dependencies: *if* there exist some tuples (which involve two variables, say ![x](doc/teximg/tex_img_61_8WUGI.png) and ![y](doc/teximg/tex_img_62_J95US.png)) satisfying such-and-such conditions, *then* ![x=y](doc/teximg/tex_img_63_ZB5I3.png). To *fire* this constraint is to take all instances of ![y](doc/teximg/tex_img_64_9PP8Q.png) and replace them with ![x](doc/teximg/tex_img_65_SBMO8.png) (or vice-versa). If both ![x](doc/teximg/tex_img_66_PQV39.png) and ![y](doc/teximg/tex_img_67_4XWM0.png) match with constants which have different values, then the chase fails.
- In both cases, there is a clear antecedent condition to check to see if the trigger is *active*.

It's possible for the sequence of instances generated by firing constraints to never terminate. For some simple chase implementations, this doesn't imply that a universal solution doesn't exist. However, more sophisticated implementations (such as the *core* chase) are complete, meaning they terminate iff there exists a solution.

## References
1. Gösta Grahne and Adrian Onet. *Anatomy of the Chase*. Fundamenta Informaticae. **157** (2018) *221–270*.
2. Alin Deustch, Alan Nash, and Jeff Remmel. *The chase revisited*. PODS (2008).