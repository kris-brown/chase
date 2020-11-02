
<!--
    To generate the readme, run:

    docker run -ti --rm -v DOLLARSIGN(pwd):/test/usr maltegruber/readme-tex:1.0.0

    see: https://github.com/MalteGruber/readme-tex

-->



# 'the chase' implementation

This contains implementations of some papers on the chase. I'll attempt to summarize the what and why of the chase below.

## Background - relational algebra

Connected with relational databases that you might be more familiar with is the concept of *relational algebra*. What we called tables are now called *relations*, which we can think of as sets of tuples of some fixed length. Importantly we can ask whether a given tuple ![t=(a,b, c, ...)](doc/teximg/tex_img_0_2RVQ8.png) is an element of a given relation ![R](doc/teximg/tex_img_1_UXO4J.png), i.e. is it the case that ![t \in R](doc/teximg/tex_img_2_08S5H.png)? We can isolate particular attributes of a tuple by projection, which we could use subscripts for: e.g. if the previous tuple was ![t](doc/teximg/tex_img_3_NR5XF.png), then ![t_2 = ``cat''](doc/teximg/tex_img_4_0DICQ.png).


![\begin{tabular}{ |c|c| } \hline \multicolumn{2}{ |c| }{Likes}   \\ \hline \textbf{Liker}  & \textbf{Likee}\\ \hline Matt            & John          \\ \hline John            & John          \\ \hline John            & Mary          \\ \hline\end{tabular}](doc/teximg/tex_img_5_2IYIN.png)


![\begin{tabular}{ |c|c| } \hline \multicolumn{2}{ |c| }{Manage}   \\ \hline \textbf{Employee}  & \textbf{Manager}\\ \hline Matt            & John          \\ \hline John            & John          \\ \hline Mary            & John          \\ \hline\end{tabular}](doc/teximg/tex_img_6_Z951I.png)


We can use first-order logic to express constraints. We can express that "`Name` is a *primary key* of `Manage`" by ![\forall x, y \in Manage: x_{Name}=y_{Name} \Rightarrow x=y](doc/teximg/tex_img_7_AFQWU.png). We can express that "`likee` is a *foreign key* ![Likes \rightarrow Manage](doc/teximg/tex_img_8_9DIJK.png)" by writing ![\forall x \in Likes: \exists mgr: (x_{Likee},\ mgr) \in {Manage}](doc/teximg/tex_img_9_BGKKB.png)

Now suppose we have a database ![I](doc/teximg/tex_img_10_MNEL9.png) and a set constraints ![\Sigma](doc/teximg/tex_img_11_WW5CJ.png), we can ask whether ![I](doc/teximg/tex_img_12_LMZ53.png) satisfies ![\Sigma](doc/teximg/tex_img_13_WX75U.png). If not, we can imagine there being the best or nearest-relative of ![I](doc/teximg/tex_img_14_5ZSK9.png) that *does* satisfy ![\Sigma](doc/teximg/tex_img_15_URWC3.png). **The chase** is an algorithm that finds this other database, given ![I](doc/teximg/tex_img_16_ANV6V.png) and ![\Sigma](doc/teximg/tex_img_17_0PT9G.png). The remaining background material to cover concerns how define this notion of a "best" relative of ![I](doc/teximg/tex_img_18_MYXZ9.png).

## Background - database homomorphisms

One interesting relationship betweeen databases is whether one is a subset of the other.

![\begin{tabular}{ |c|c| } \hline \multicolumn{2}{ |c| }{R}   \\ \hline \textbf{1}  & \textbf{2}\\ \hline 200            & 200          \\ \hline\end{tabular}](doc/teximg/tex_img_19_YKAAO.png)

![\begin{tabular}{ |c|c| } \hline \multicolumn{2}{ |c| }{Q} \\ \hline \textbf{1}  & \textbf{2}  \\ \hline 7              & 8        \\ \hline 200            & 200      \\ \hline\end{tabular}](doc/teximg/tex_img_20_DKYH4.png)


We'll consider single-relation database instances in this section to keep the examples small, which allows us to abuse notation and treat the relations as databases in their own right. In the above examples, we could say ![R \subset Q](doc/teximg/tex_img_21_0EPLB.png) to mean one database is a subset of the other.

Although all example databases above have been fully specified, generally databases in this formalism can have "labeled nulls" which behave like variables.


![\begin{tabular}{ |c|c| } \hline \multicolumn{2}{ |c| }{S} \\ \hline \textbf{1}  & \textbf{2}  \\ \hline a            & a        \\ \hline b            & 200      \\ \hline\end{tabular}](doc/teximg/tex_img_22_GGFRQ.png)

This new instance ![S](doc/teximg/tex_img_23_88RLK.png) says there's a tuple that has two of the same unknown value, and another which has an unknown first value and ![200](doc/teximg/tex_img_24_57757.png) for the second value. We need to refine our notion of ![\subset](doc/teximg/tex_img_25_416G8.png) in light of the possibility of labeled nulls. The relevant relation now is called a *homomorphism* (![A \rightarrow B](doc/teximg/tex_img_26_12OSJ.png)) which consists in an assignment to the labeled nulls in ![A](doc/teximg/tex_img_27_QRO7X.png) such that, when substituting ![A](doc/teximg/tex_img_28_M5D2Q.png) with this assignment, we get a database instance that is literally a subset of ![B](doc/teximg/tex_img_29_E4W5W.png). For example, the assignment ![\{a \mapsto 200, b \mapsto 200\}](doc/teximg/tex_img_30_26XVF.png) is a homomorphism ![S \rightarrow R](doc/teximg/tex_img_31_GLXC4.png). There is no homomorphism ![R \rightarrow S](doc/teximg/tex_img_32_QNT5B.png). (![R \rightarrow Q](doc/teximg/tex_img_33_XKA53.png) is a homomorphism trivially, no substitution needed; for any ![A](doc/teximg/tex_img_34_W9OP4.png) we have ![A \rightarrow A](doc/teximg/tex_img_35_E8GQG.png), given we can make an identity substitution).

Even with merely a simple binary relation like ![\rightarrow](doc/teximg/tex_img_36_7L2R1.png), we can derive a notion of the 'closest' related ![U](doc/teximg/tex_img_37_A2E60.png) for some given ![I](doc/teximg/tex_img_38_ED3AQ.png). Fix a given ![\Sigma](doc/teximg/tex_img_39_OXYLL.png) of constraints and imagine the set of all possible instances which both satisfy ![\Sigma](doc/teximg/tex_img_40_DQ081.png) and are related to ![I](doc/teximg/tex_img_41_UQUPM.png). Among this set, it may be the case that there exists a ![U](doc/teximg/tex_img_42_AU1AG.png) such that ![I \rightarrow U \rightarrow X](doc/teximg/tex_img_43_BFTEH.png) for all ![X](doc/teximg/tex_img_44_YIUMT.png) instances in this set, i.e. *any* homomorphism can be factored through the homomorphism with ![U](doc/teximg/tex_img_45_Q0YDQ.png). (analgously, consider the relation ![\le](doc/teximg/tex_img_46_Q822C.png) for integers, let ![I=12](doc/teximg/tex_img_47_80SG3.png) and ![\Sigma = \{> 14, < 20\}](doc/teximg/tex_img_48_0HIO4.png) ... we want something that is 'related' to ![I](doc/teximg/tex_img_49_IRJ2O.png), so we are looking for an ![i](doc/teximg/tex_img_50_GBRV8.png) such that ![12 \le i](doc/teximg/tex_img_51_CGNOO.png) which also satisfies the constraints. There exist 5 possible candidates (![15](doc/teximg/tex_img_52_RVZ3A.png) through ![19](doc/teximg/tex_img_53_EJV4P.png) inclusive) and it happens that for any candidate ![X](doc/teximg/tex_img_54_DA509.png) we can write ![I \le U \le X](doc/teximg/tex_img_55_T0KTM.png) with the value of ![U=15](doc/teximg/tex_img_56_7P4E4.png). Thus, ![15](doc/teximg/tex_img_57_8B8LS.png) is the 'closest' integer to ![12](doc/teximg/tex_img_58_ZIX33.png) that satisfies the constraints).

The database instance ![U](doc/teximg/tex_img_59_40IL6.png) that satisfies this property is called *universal* (with respect to ![I,\  \Sigma](doc/teximg/tex_img_60_ZB5KE.png)), and this is the instance that the chase returns, if it exists.

## Why
Reading about what the chase is and how it works will be more interesting with context as to why finding a universal database instance is of interest.
- Query Optimization
- Constraint Implication
- Universal Solutions
- Certain Answers


## What
The simplest variation of the chase considers each constraint as a potential trigger which can be *fired* to produce a new instance that satisfies that constraint. We then iteratively repeat this process until there exist no active triggers (i.e. produce a sequence of instances until we arrive at one which satisfies the constraints).

The literature is generally concerned with two types of constraints.
- Tuple-generating dependencies: *if* there exist some tuples satisfying such-and-such conditions, *then* there must exist these other tuples satisfying such-and-such conditions. To *fire* this constraint is to add the tuples in the consequent clause.
- Equality-generating dependnecies: *if* there exist some tuples (which involve two variables, say ![x](doc/teximg/tex_img_61_2M0TM.png) and ![y](doc/teximg/tex_img_62_6BQSO.png)) satisfying such-an-dsuch conditions, *then* ![x=y](doc/teximg/tex_img_63_DYCCP.png). To *fire* this constraint is to take all instances of ![y](doc/teximg/tex_img_64_L0GU3.png) and replace them with ![x](doc/teximg/tex_img_65_P2Z41.png) (or vice-versa). If both ![x](doc/teximg/tex_img_66_PKAP0.png) and ![y](doc/teximg/tex_img_67_8LYPM.png) match with constants which have different values, then the chase fails.
- In both cases, there is a clear antecedent condition to check to see if the trigger is *active*.

It's possible for the sequence of instances generated by firing constraints to never terminate. For some simple chase implementations, this doesn't imply that a universal solution doesn't exist. However, more sophisticated implementations (such as the *core* chase) are complete, meaning they terminate iff there exists a solution.

## References
1. Gösta Grahne and Adrian Onet. *Anatomy of the Chase*. Fundamenta Informaticae. **157** (2018) *221–270*.
2.