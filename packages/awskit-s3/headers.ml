include
  Awskit_s3_header_helpers.Make
    (struct
      module Account_id = Account_id
      module Content_type = Content_type
      module Tag = Tag
      module Storage_class = Storage_class
      module Encryption = Encryption
      module Object = Object
    end)
    (struct
      let ptime_to_header = S3_time.to_header
    end)
