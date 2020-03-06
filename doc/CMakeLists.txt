set (LANGUAGES
  en
  )

set (common_SRCS
  common/communication.adoc
  common/license.adoc
  common/links.adoc
  )

set (UG_SRCS
  docinfo.html
  docinfo.xml
  acknowledgements.adoc
  compiling.adoc
  config-details.adoc
  controls-functions-center.adoc
  controls-functions-left.adoc
  controls-functions-main-window.adoc
  controls-functions-menus.adoc
  controls-functions-messages.adoc
  controls-functions-status-bar.adoc
  controls-functions-wide-graph.adoc
  cooperating-programs.adoc
  faq.adoc
  font-sizes.adoc
  implementation.adoc
  install-from-source.adoc
  install-linux.adoc
  install-mac.adoc
  install-windows.adoc
  introduction.adoc
  protocols.adoc
  logging.adoc
  make-qso.adoc
  new_features.adoc
  platform-dependencies.adoc
  settings-audio.adoc
  settings-colors.adoc
  settings-frequencies.adoc
  settings-general.adoc
  settings-radio.adoc
  settings-reporting.adoc
  settings-txmacros.adoc
  support.adoc
  system-requirements.adoc
  transceiver-setup.adoc
  tutorial-example1.adoc
  tutorial-example2.adoc
  tutorial-main-window.adoc
  tutorial-wide-graph-settings.adoc
  utilities.adoc
  vhf-features.adoc
  wsjtx-main.adoc
  wspr.adoc
  )

set (UG_IMGS
  images/130610_2343-wav-80.png
  images/band-settings.png
  images/colors.png
  images/decode-menu.png
  images/decodes.png
  images/file-menu.png
  images/freemsg.png
  images/help-menu.png
  images/jtalert.png
  images/keyboard-shortcuts.png
  images/log-qso.png
  images/MacAppMenu.png
  images/main-ui-1.5.png
  images/main-ui-controls.png
  images/misc-controls-center.png
  images/misc-main-ui.png
  images/mode-menu.png
  images/new-msg-box.png
  images/psk-reporter.png
  images/r3666-config-screen-80.png
  images/r3666-main-ui-80.png
  images/r4148-txmac-ui.png
  images/RadioTab.png
  images/reporting.png
  images/save-menu.png
  images/settings-audio.png
  images/settings-frequencies.png
  images/settings-general.png
  images/setup-menu.png
  images/special-mouse-commands.png
  images/status-bar-a.png
  images/traditional-msg-box.png
  images/tx-macros.png
  images/view-menu.png
  images/wide-graph-controls.png

)

find_program (ASCIIDOCTOR_EXECUTABLE NAMES asciidoctor)
if (NOT ASCIIDOCTOR_EXECUTABLE)
  message (FATAL_ERROR "asciidoctor is required to build the documentation

Building the documenation may optionally be turned off by setting the CMake
option WSJT_GENERATE_DOCS to OFF.")
endif (NOT ASCIIDOCTOR_EXECUTABLE)

find_program (FOPUB_EXECUTABLE NAMES fopub)

include (CMakeParseArguments)

# generate a document from asciidoc text files(s)
#
# HTML - generate an HTML document
# PDF - generate a PDF document
# SOURCE - top level asciidoc file
# ASCIIDOCTOR_OPTIONS - asciidoctor command options
# DEPENDS - dependent files
function (document)
  cmake_parse_arguments (_args "HTML" "SOURCE;LANG;OUTPUT" "ASCIIDOCTOR_OPTIONS;PDF;DEPENDS" ${ARGN})
  get_filename_component (_source_path ${_args_SOURCE} PATH)
  get_filename_component (_source_name ${_args_SOURCE} NAME)
  get_filename_component (_output_name_we ${_args_SOURCE} NAME_WE)

  # HTML
  if (${_args_HTML})
    set (_html_file ${CMAKE_CURRENT_BINARY_DIR}/${_output_name_we}_${lang}.html)
    add_custom_command (
      OUTPUT ${_html_file}
      WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/${_source_path}/${lang}
      COMMAND ${ASCIIDOCTOR_EXECUTABLE} ${_args_ASCIIDOCTOR_OPTIONS}
      -b html5
      -a VERSION_MAJOR=${WSJTX_VERSION_MAJOR}
      -a VERSION_MINOR=${WSJTX_VERSION_MINOR}
      -a VERSION_PATCH=${WSJTX_VERSION_PATCH}
      -a VERSION=${wsjtx_VERSION}
      --out-file=${_html_file} ${_source_name}
      DEPENDS ${_args_DEPENDS}
      COMMENT "Generating ${_html_file}"
      )
    list (APPEND _output_files ${_html_file})
  endif (${_args_HTML})

  # PDF
  if (_args_PDF AND EXISTS ${FOPUB_EXECUTABLE})
    set (_docbook_file ${CMAKE_CURRENT_BINARY_DIR}/${_output_name_we}_${lang}.xml)
    set (_pdf_file_we ${CMAKE_CURRENT_BINARY_DIR}/${_output_name_we}_${lang})
    add_custom_command (
      OUTPUT ${_docbook_file} "${_pdf_file_we} (USLetter).pdf" "${_pdf_file_we} (A4).pdf"
      WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/${_source_path}/${lang}
      COMMAND ${ASCIIDOCTOR_EXECUTABLE} ARGS ${_args_ASCIIDOCTOR_OPTIONS}
      -b docbook
      -a data-uri!
      -a VERSION_MAJOR=${WSJTX_VERSION_MAJOR}
      -a VERSION_MINOR=${WSJTX_VERSION_MINOR}
      -a VERSION_PATCH=${WSJTX_VERSION_PATCH}
      -a VERSION=${wsjtx_VERSION}
      -D ${CMAKE_CURRENT_BINARY_DIR}
      -o ${_docbook_file} ${_source_name}
      COMMAND ${FOPUB_EXECUTABLE} ARGS ${_docbook_file} ${_args_PDF} -param paper.type USLetter
      COMMAND ${CMAKE_COMMAND} ARGS -E rename ${_pdf_file_we}.pdf '${_pdf_file_we} \(USLetter\).pdf'
      COMMAND ${FOPUB_EXECUTABLE} ARGS ${_docbook_file} ${_args_PDF} -param paper.type A4
      COMMAND ${CMAKE_COMMAND} ARGS -E rename ${_pdf_file_we}.pdf '${_pdf_file_we} \(A4\).pdf'
      DEPENDS ${_args_DEPENDS}
      COMMENT "Generating ${_pdf_file_we}.pdf"
      )
    list (APPEND _output_files "${_pdf_file_we} (USLetter).pdf" "${_pdf_file_we} (A4).pdf")
  endif (_args_PDF AND EXISTS ${FOPUB_EXECUTABLE})

  set (${_args_OUTPUT} ${_output_files} PARENT_SCOPE)
endfunction (document)

set (htmls)
set (pdfs)
foreach (lang ${LANGUAGES})
  set (_sources)
  foreach (_src ${UG_SRCS} ${UG_IMGS})
    list (APPEND _sources "user_guide/${lang}/${_src}")
  endforeach ()
  document(
    HTML
    SOURCE user_guide/wsjtx-main.adoc
    LANG "${lang}"
    OUTPUT html
    ASCIIDOCTOR_OPTIONS -d book -a data-uri -a toc=left -a max-width=1024px
    DEPENDS ${common_SRCS} ${_sources}
    )
  document(
    PDF -param body.font.master 11 -param body.font.family "'Noto Sans, Helvetica, sans-serif'" -param title.font.family "'Noto Serif, Times New Roman, serif'" -param page.margin.inner 1cm -param page.margin.outer 1cm -param page.margin.top 0.75cm -param page.margin.bottom 0.5cm -param generate.toc 0
    SOURCE user_guide/wsjtx-main.adoc
    LANG "${lang}"
    OUTPUT pdf
    ASCIIDOCTOR_OPTIONS -d book
    DEPENDS ${common_SRCS} ${_sources}
    )
  list (APPEND htmls "${html}")
  list (APPEND pdfs "${pdf}")
endforeach ()

add_custom_target (docs ALL DEPENDS ${htmls} ${pdfs})

foreach (_html ${htmls})
  get_filename_component (_path ${_html} PATH)
  get_filename_component (_nwe ${_html} NAME_WE)
  get_filename_component (_ext ${_html} EXT)
  string (REGEX REPLACE "_en$" "" _nwe ${_nwe})
  install (FILES
    ${_html}
    DESTINATION ${CMAKE_INSTALL_DOCDIR}
    RENAME ${_nwe}-${wsjtx_VERSION}${_ext}
    #COMPONENT runtime
    )
endforeach ()