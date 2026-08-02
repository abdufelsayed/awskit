(** Domain values returned by one physical S3 attempt.

    These values are consumed by the retry kernel. Observation code may record
    them, but it never creates or chooses a transition. *)

open Base

type 'a response =
  | Success of ('a, Awskit.Error.t) Result.t
  | Retry of Awskit.Error.t

type 'a result =
  | Complete of ('a, Awskit.Error.t) Result.t
  | Retry_after of {
      budget_state : Awskit.Retry.budget_state;
      delay : Ptime.Span.t;
      error : Awskit.Error.t;
    }
