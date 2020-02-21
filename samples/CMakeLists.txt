set (SAMPLE_FILES
  JT9+JT65/130610_2343.wav
  JT9/130418_1742.wav
  )

set (contents_file_name contents_${WSJTX_VERSION_MAJOR}.${WSJTX_VERSION_MINOR}.json)
set (contents_file ${CMAKE_CURRENT_BINARY_DIR}/${contents_file_name})
set (web_tree ${CMAKE_CURRENT_BINARY_DIR}/web)
set_source_files_properties (${contents_file} PROPERTIES GENERATED ON)

add_custom_command (
  OUTPUT ${contents_file}
  COMMAND ${CMAKE_COMMAND} ARGS -Dcontents_file=${contents_file} -DFILES="${SAMPLE_FILES}" -DDEST=${CMAKE_CURRENT_BINARY_DIR} -P make_contents.cmake
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  DEPENDS ${SAMPLE_FILES} make_contents.cmake
)

find_program (RSYNC_EXECUTABLE rsync)
add_custom_command (
  OUTPUT upload.timestamp
  COMMAND ${CMAKE_COMMAND} ARGS -E copy_if_different ${contents_file} ${web_tree}/samples/${contents_file_name}
  COMMAND ${RSYNC_EXECUTABLE} ARGS -avz --progress ${CMAKE_CURRENT_BINARY_DIR}/web/samples ${PROJECT_SAMPLES_UPLOAD_DEST}
  COMMAND ${CMAKE_COMMAND} ARGS -E touch upload.timestamp
  DEPENDS ${contents_file}
  COMMENT "Uploading WSJT-X samples to web server"
  )
add_custom_target (upload-samples DEPENDS upload.timestamp)
