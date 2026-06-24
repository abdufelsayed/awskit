include
  Awskit_s3_header_helpers.Make
    (struct
      module Account_id = Awskit_s3.Account_id
      module Content_type = Awskit_s3.Content_type
      module Tag = Awskit_s3.Tag
      module Storage_class = Awskit_s3.Storage_class
      module Object = Awskit_s3.Object
    end)
    (struct
      let ptime_to_header = Simulator_support.ptime_to_header
      let validate_header_value = Simulator_support.validate_header_value
    end)
