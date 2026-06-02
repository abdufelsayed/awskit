module Public = struct
  include Sim_support
  module Multipart = Sim_multipart.Multipart

  module Object = struct
    include Sim_object.Object
  end

  module Bucket = Sim_bucket.Bucket
  module Presigned = Sim_presigned.Presigned
end
