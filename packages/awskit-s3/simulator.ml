module Sim = struct
  include Sim_support
  module Object = Sim_object.Object
  module Bucket = Sim_bucket.Bucket
  module Multipart = Sim_multipart.Multipart
  module Presigned = Sim_presigned.Presigned
end
