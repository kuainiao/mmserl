
DROP TABLE IF EXISTS `mms_file`;

CREATE TABLE `mms_file` (
  `id` varbinary(64) NOT NULL,
  `uid` varbinary(64) NOT NULL,
  `filename` varbinary(250) NOT NULL,
  `type` tinyint unsigned NOT NULL DEFAULT '1',
  `owner` varbinary(250) NOT NULL,
  `created_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;


DROP TABLE IF EXISTS `mms_multipart`;

CREATE TABLE `mms_multipart` (
  `upload_id` varbinary(64) NOT NULL,
  `fileid` varbinary(64) NOT NULL,
  `uid` varbinary(64) NOT NULL,
  `type` tinyint unsigned NOT NULL,
  `created_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (`upload_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;


DROP TABLE IF EXISTS `mms_multipart_records`;

CREATE TABLE `mms_multipart_records` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `upload_id` varbinary(64) NOT NULL,
  `part_number` smallint unsigned NOT NULL,
  `etag` varbinary(64) NOT NULL,
  `created_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  UNIQUE KEY (upload_id,part_number),
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;