Compiling Without Z3
====================

To build on on platforms WITHOUT Z3, do this:

	% cp tpNull.ml.noz3 tpNull.ml

and then

	% make


To build on the mac (no Z3), do 

	% make -f Makefile.mac

To build on platforms WITH Z3, do this:

	% cp tpNull.ml.z3 tpNull.ml
	% make


Constraint Files
================

The general format of a constraint file is:

  [SUBTYPING CONSTRAINTS]

  [WELL-FORMEDNESS CONSTRAINTS]
  
  [SOLUTIONS]
  
Subtyping Constraints
---------------------

A subtyping constraint has the form

  constraint:
    env[BINDINGS]
    grd PREDICATE
    lhs REFTYPE
    rhs REFTYPE
    id INT tag INT_LIST
    
The BINDINGS are a list of semicolon-separated name, REFTYPE
pairs. The guard PREDICATE is a standard predicate. The integer ID is
used to identify this constraint in error messages, etc.  

The `INT_LIST` is used as a hook into constraint ordering: 
if two constraints are equal according to the predicate 
variable dependency graph, the tie is broken by comparing
the tags in lexicographic order.

Refined Types
-------------

A refined type has the form

    {VALUE_VAR : SORT | [PREDICATE_OR_KVARS]}

The `VALUE_VAR` is an identifier. 
The SORT is one of int, bool, or ptr.
The list `PREDICATE_OR_KVARS` is a semicolon 
separated list of predicates and/or kvars (identifiers).

Well-Formedness Constraints
---------------------------

A well-formedness constraint has the form

  wf:
    env[BINDINGS]
    reft REFTYPE

Solutions
---------

A solution assigns a predicate variable to a 
semicolon-separated list of predicates:

  solution: KVAR := [PREDICATES]

TpSmtLib
========

Split: 
    tpZ3Gen  = tpSmt + z3-instance
    tpSMTLib = tpSmt + smtlib-instance


1. tpGen.ml + smtZ3
            + smtCVC
            + smtLIB
            + smtALTERGO

3. generalize into a SIG, make tp-* be parameterized by SIG
4. write the smtlib instantiation of SIG

Conversion to SMTLIB2 Horn Clauses
==================================



This

    :extrapreds ((k_120 Int Int Int Int Int Int Int Int Int Int Int))

Becomes

    (declare-fun k_120
               (Int
                Int
                Int
                Int
                Int
                Int
                Int
                Int
                Int
                Int
                Int
                (Array Int Int)
                (Array Int (Array Int Int))
                (Array Int Bool)
                (Array Int Int))
               Bool)


2. This [global]

         ; constant 
         :extrafuns ((len Int Int))

         ; constant 
         :extrafuns ((cmp Int Int))


  Becomes [local]

         (len (Array Int Int))

         (cmp (Array Int Int)))

3. This 

        (cmp EQ_6U)

    Becomes

        (select cmp EQ_6U)

4. This constraint 


    ; cid = 16
    :assumption
    (implies ((and (k_241 EQ_6U False_68 GT_6W LT_6S True_6u lq_tmp_x15 lq_anf__dC1 m_ruj n_rui realWorld__0f) (and (k_243 EQ_6U False_68 GT_6W LT_6S True_6u lq_tmp_x16 lq_anf__dC1 m_ruj n_rui realWorld__0f) (and (k_61 EQ_6U False_68 GT_6W LT_6S True_6u m_ruj realWorld__0f) (and (k_65 EQ_6U False_68 GT_6W LT_6S True_6u n_rui m_ruj realWorld__0f) (and (= (cmp EQ_6U) EQ_6U) (and (not (Prop False_68)) (and (= (cmp GT_6W) GT_6W) (and (= (cmp LT_6S) LT_6S) (and (Prop True_6u) (and (= lq_anf__dC1 0) (and (= VV_F16 (+ lq_tmp_x15 lq_tmp_x16)) true)))))))))))) (k_241 EQ_6U False_68 GT_6W LT_6S True_6u VV_F16 lq_anf__dC1 m_ruj n_rui realWorld__0f))
    
    Becomes

    (assert (forall ((True_6u Int)
             (EQ_6U Int)
             (False_68 Int)
             (GT_6W Int)
             (VV_F16 Int)
             (lq_tmp_x16 Int)
             (m_ruj Int)
             (realWorld__0f Int)
             (lq_anf__dC1 Int)
             (LT_6S Int)
             (lq_tmp_x15 Int)
             (n_rui Int)
             (len (Array Int Int))
             (fix__58__35_64 (Array Int (Array Int Int)))
             (Prop (Array Int Bool))
             (cmp (Array Int Int)))
      (=> (and (k_241 EQ_6U
                      False_68
                      GT_6W
                      LT_6S
                      True_6u
                      lq_tmp_x15
                      lq_anf__dC1
                      m_ruj
                      n_rui
                      realWorld__0f
                      len
                      fix__58__35_64
                      Prop
                      cmp)
               (k_243 EQ_6U
                      False_68
                      GT_6W
                      LT_6S
                      True_6u
                      lq_tmp_x16
                      lq_anf__dC1
                      m_ruj
                      n_rui
                      realWorld__0f
                      len
                      fix__58__35_64
                      Prop
                      cmp)
               (k_61 EQ_6U
                     False_68
                     GT_6W
                     LT_6S
                     True_6u
                     m_ruj
                     realWorld__0f
                     len
                     fix__58__35_64
                     Prop
                     cmp)
               (k_65 EQ_6U
                     False_68
                     GT_6W
                     LT_6S
                     True_6u
                     n_rui
                     m_ruj
                     realWorld__0f
                     len
                     fix__58__35_64
                     Prop
                     cmp)
               (= (select cmp EQ_6U) EQ_6U)
               (not (select Prop False_68))
               (= (select cmp GT_6W) GT_6W)
               (= (select cmp LT_6S) LT_6S)
               (select Prop True_6u)
               (= lq_anf__dC1 0)
               (= VV_F16 (+ lq_tmp_x15 lq_tmp_x16))
               true)
          (k_241 EQ_6U
                 False_68
                 GT_6W
                 LT_6S
                 True_6u
                 VV_F16
                 lq_anf__dC1
                 m_ruj
                 n_rui
                 realWorld__0f
                 len
                 fix__58__35_64
                 Prop
                 cmp))))

