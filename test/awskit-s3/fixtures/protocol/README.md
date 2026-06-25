# S3 Protocol Fixtures

These fixtures are committed evidence for Awskit's S3 protocol behavior. They
cover observable SDK outputs and parsed service inputs: presigned artifacts,
endpoint resolution, XML payloads, pagination, multipart completion, service
errors, and minimized replay cases.

Fixtures normalize nondeterministic or secret-bearing values before comparison.
Presigned URLs redact signature and credential query values. Fuzz replay cases
are ordinary deterministic tests; long-running mutation fuzzing remains opt-in
until a separate corpus, minimization, and resource policy exists.

Treat fixture payloads as golden test data. Update them together with the
focused test that proves the new supported wire behavior, not as general prose
cleanup.
