/*
Schema of version 1.
*/

/*
Encoins table.
*/
CREATE TABLE IF NOT EXISTS encoins (
  "id" serial not null primary key,
  "asset_name" text not null,
  "encrypted_secret" text not null,
  "save_time" bigint not null,
  UNIQUE(asset_name, encrypted_secret)
);
