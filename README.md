Question: Consider a multi-armed bandit with k = 2 arms and n = 200
time periods. Rewards Xt,a are uniformly distributed with support [la, ua],
for unknown 0 ≤ la < ua ≤ 1, a = 1, 2.
(a) Design a good possible policy based on your own ideas: determine for
each t = 1, . . . , n a (possibly random) action At based solely on the
available data (As, Xs)s≤t−1, and not on future data or on the true
values of µa. Carefully describe the underlying ideas and intuition,
and why you believe this is a good policy. Give both an intuitive
and a formal definition of the policy. Try to have the best possible
performance.
(b) Implement the policy and report its instance-dependent regret after n
time periods, for all 100 feasible values of l1, u1, l2, u2 ∈ {0.1, 0.3, 0.5, 0.7, 0.9},
including convincing confidence bounds. Hint: to speed up implementation you may use Additional Exercise 2.
(b2) As a sanity check, also implement both fixed-action policies that use
the same action in all time periods. Report their regret for all feasible values of l1, u1, l2, u2 ∈ {0.1, 0.3, 0.5, 0.7, 0.9}, including confidence
bounds. Explain if/whether the observed values are conform what you
would expect.
(c) What is the worst-case regret that you observe, and at which value of
l1, u1, l2, u2 is it attained? Explain the intuition.
