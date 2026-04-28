module Sim = struct
  include Awskit_s3_sim_support
  module Object = Awskit_s3_sim_object.Object
  module Bucket = Awskit_s3_sim_bucket.Bucket
  module Multipart = Awskit_s3_sim_multipart.Multipart
  module Presigned = Awskit_s3_sim_presigned.Presigned
end
