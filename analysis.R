conn <- RPostgreSQL::dbConnect(
  "PostgreSQL", host = "localhost", port = 5432,
  dbname = "gis", user = "gisuser", password = "gisuser"
)

#
x <- st_read(conn, query = "select * from postcode where not st_isvalid(st_makevalid(geom))")

# intersection of postcode and meshblock ---------------------------------------
system.time({
  mb_int_postcode <- st_read(
    conn,
    query = "
    select
      a.code as mb,
      b.code as postcode,
      ST_INTERSECTION(a.geom, st_makevalid(b.geom)) as geom
    from
      mb a
    inner join
      postcode b
    on
      ST_INTERSECTS(a.geom, st_makevalid(b.geom))
    "
  )
}) # about 8 minutes # 509 seconds

# just keep the one with the largest area
mbpostcode <- mb_int_postcode |>
  mutate(area = st_area(geom)) |>
  arrange(mb, desc(area)) |>
  group_by(mb) |>
  filter(row_number() == 1) |>
  select(-area) |>
  ungroup() |>
  st_drop_geometry() |>
  as.data.frame()

postcode_mb <- mb |>
  inner_join(mbpostcode, by = c("code" = "mb")) |>
  group_by(postcode) |> summarise(num_mb = n())

st_write(postcode_mb, conn, c("public", "postcode_mb"))

DBI::dbSendQuery(
  conn,
  '
  CREATE INDEX postcodemb_idx
  ON postcode_mb
  USING GIST (geom);
  '
)

# attach MB to LINZ addresses --------------------------------------------
system.time({
  addr_w_mb <- DBI::dbGetQuery(
    conn,
    "
    select
      a.address_id, b.code as mb
    from
      address a
    left outer join
      mb b
    on
      ST_CONTAINS(b.geom, a.geom)
    "
  )
}) # about 39 seconds

# attach postcode to LINZ addresses --------------------------------------------
system.time({
  addr_w_postcode <- DBI::dbGetQuery(
    conn,
    "
    select
      a.address_id, b.code as postcode
    from
      address a
    left outer join
      postcode b
    on
      ST_CONTAINS(b.geom, a.geom)
    "
  )
})







# attach postcode to LINZ address ----------------------------------------------
system.time({
  addr_w_postcode <- st_read(
    conn,
    query = "
    select
      a.*, b.code as postcode, b.description as postcode_name
    from
      address a
    left outer join
      postcode b
    on
      ST_CONTAINS(b.geom, a.geom)
    "
  )
})



509




DBI::dbGetQuery(conn, "show max_worker_processes;")
DBI::dbGetQuery(conn, "show max_parallel_workers;")
DBI::dbGetQuery(conn, "show max_parallel_workers_per_gather;")

DBI::dbSendQuery(conn, "SET max_parallel_workers = 8;")
DBI::dbSendQuery(conn, "SET max_parallel_workers_per_gather = 4;")
