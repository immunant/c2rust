#! /usr/bin/env python3
# -*- coding: utf-8 -*-

from common import Config
from plumbum.cmd import sed
from plumbum import local
from transpile import transpile_files
from typing import Iterable, Tuple

import argparse
import os

desc = 'transpile files in compiler_commands.json.'
parser = argparse.ArgumentParser(description="Translates libxml2 into the repo/rust/src directory")
# parser.add_argument('-i', '--interactive',
#                     default=False, action='store_true',
#                     help='Interactively choose which patches to apply')

config = Config()

C2RUST_DIR = config.ROOT_DIR
LIBXML2_REPO = os.path.join(C2RUST_DIR, "examples/libxml2/repo")
RUST_ROOT_DIR = os.path.join(LIBXML2_REPO, "rust")
RUST_SRC_DIR = os.path.join(RUST_ROOT_DIR, "src")
PATCHES = {
    "threads.rs": {
        "replace_all": [
            ("unsafe { 1i32.wrapping_neg() }", "-1"),
        ],
    },
    "xmlunicode.rs": {
        "replace_all_null_ptr_cast": [
            "xmlUnicodeBlocks.as_ptr()",
            "xmlUnicodeCats.as_mut_ptr()",
            "xmlZS.as_ptr()",
            "xmlSoL.as_ptr()",
            "xmlSoS.as_ptr()",
            "xmlSmL.as_ptr()",
            "xmlSmS.as_ptr()",
            "xmlSkS.as_ptr()",
            "xmlScS.as_ptr()",
            "xmlSL.as_ptr()",
            "xmlSS.as_ptr()",
            "xmlPsS.as_ptr()",
            "xmlPoL.as_ptr()",
            "xmlPoS.as_ptr()",
            "xmlPeS.as_ptr()",
            "xmlPdS.as_ptr()",
            "xmlPL.as_ptr()",
            "xmlPS.as_ptr()",
            "xmlNoL.as_ptr()",
            "xmlNoS.as_ptr()",
            "xmlNdL.as_ptr()",
            "xmlNdS.as_ptr()",
            "xmlNL.as_ptr()",
            "xmlNS.as_ptr()",
            "xmlMnL.as_ptr()",
            "xmlMnS.as_ptr()",
            "xmlMcL.as_ptr()",
            "xmlMcS.as_ptr()",
            "xmlML.as_ptr()",
            "xmlMS.as_ptr()",
            "xmlLuL.as_ptr()",
            "xmlLuS.as_ptr()",
            "xmlLtS.as_ptr()",
            "xmlLoL.as_ptr()",
            "xmlLoS.as_ptr()",
            "xmlLmS.as_ptr()",
            "xmlLlL.as_ptr()",
            "xmlLlS.as_ptr()",
            "xmlLL.as_ptr()",
            "xmlLS.as_ptr()",
            "xmlCfL.as_ptr()",
            "xmlCfS.as_ptr()",
            "xmlCL.as_ptr()",
            "xmlCS.as_ptr()",
        ],
    },
    "chvalid.rs": {
        "replace_all_null_ptr_cast": [
            "xmlIsIdeographic_srng.as_ptr()",
            "xmlIsExtender_srng.as_ptr()",
            "xmlIsDigit_srng.as_ptr()",
            "xmlIsCombining_srng.as_ptr()",
            "xmlIsChar_lrng.as_ptr()",
            "xmlIsChar_srng.as_ptr()",
            "xmlIsBaseChar_srng.as_ptr()",
        ],
    },
    "HTMLparser.rs": {
        "replace_all_null_ptr": [
            "html_attrs.as_ptr()",
            "html_inline.as_ptr()",
            "ul_depr.as_ptr()",
            " li_elt.as_ptr()",
            "bgcolor_attr.as_ptr()",
            "talign_attrs.as_ptr()",
            "tr_contents.as_ptr()",
            " i18n_attrs.as_ptr()",
            "html_pcdata.as_ptr()",
            "talign_attrs.as_ptr()",
            "tr_elt.as_ptr()",
            "th_td_depr.as_ptr()",
            "th_td_attr.as_ptr()",
            "html_flow.as_ptr()",
            "rows_cols_attr.as_ptr()",
            "textarea_attrs.as_ptr()",
            "table_depr.as_ptr()",
            "table_attrs.as_ptr()",
            "table_contents.as_ptr()",
            "type_attr.as_ptr()",
            "style_attrs.as_ptr()",
            "select_attrs.as_ptr()",
            "select_content.as_ptr()",
            "type_attr.as_ptr()",
            "language_attr.as_ptr()",
            "script_attrs.as_ptr()",
            "quote_attrs.as_ptr()",
            "width_attr.as_ptr()",
            "pre_content.as_ptr()",
            "name_attr.as_ptr()",
            "param_attrs.as_ptr()",
            "align_attr.as_ptr()",
            "option_attrs.as_ptr()",
            "label_attr.as_ptr()",
            "optgroup_attrs.as_ptr()",
            "option_elt.as_ptr()",
            " ol_attrs.as_ptr()",
            "object_depr.as_ptr()",
            "object_attrs.as_ptr()",
            "object_contents.as_ptr()",
            "noframes_content.as_ptr()",
            "content_attr.as_ptr()",
            "meta_attrs.as_ptr()",
            "compact_attrs.as_ptr()",
            "blockli_elt.as_ptr()",
            "name_attr.as_ptr()",
            "map_contents.as_ptr()",
            "target_attr.as_ptr()",
            "link_attrs.as_ptr()",
            "align_attr.as_ptr()",
            "legend_attrs.as_ptr()",
            "label_attrs.as_ptr()",
            "prompt_attrs.as_ptr()",
            "edit_attrs.as_ptr()",
            " a_attrs.as_ptr()",
            "inline_p.as_ptr()",
            "flow_param.as_ptr()",
            "applet_attrs.as_ptr()",
            "area_attrs.as_ptr()",
            "alt_attr.as_ptr()",
            "href_attrs.as_ptr()",
            "basefont_attrs.as_ptr()",
            "core_i18n_attrs.as_ptr()",
            "dir_attr.as_ptr()",
            "body_contents.as_ptr()",
            "body_attrs.as_ptr()",
            "body_depr.as_ptr()",
            "core_attrs.as_ptr()",
            "clear_attrs.as_ptr()",
            "button_attrs.as_ptr()",
            "col_attrs.as_ptr()",
            "col_elt.as_ptr()",
            "col_attrs.as_ptr()",
            "dl_contents.as_ptr()",
            "compact_attr.as_ptr()",
            "embed_attrs.as_ptr()",
            "fieldset_contents.as_ptr()",
            " font_attrs.as_ptr()",
            "form_contents.as_ptr()",
            "form_attrs.as_ptr()",
            "action_attr.as_ptr()",
            " frame_attrs.as_ptr()",
            "iframe_attrs.as_ptr()",
            "frameset_contents.as_ptr()",
            "frameset_attrs.as_ptr()",
            "head_contents.as_ptr()",
            "head_attrs.as_ptr()",
            "hr_depr.as_ptr()",
            "html_content.as_ptr()",
            "version_attr.as_ptr()",
            "img_attrs.as_ptr()",
            "src_alt_attrs.as_ptr()",
            "input_attrs.as_ptr()",
        ]
    },
}

# FIXME: Still need to initialize the above pointers

# TODO: Better error handling
def replace_all(file_path: str, replacements: Iterable[Tuple[str, str]]) -> None:
    sed_args = ["-i"]

    for replace_from, replace_to in replacements:
        sed_args.append("-e")
        sed_args.append("s/{}/{}/".format(replace_from, replace_to))

    sed_args.append(file_path)

    retcode, stdout, stderr = sed[sed_args].run()

    assert retcode != 1, "Failed to apply patch {}/replace_all:\n{}".format(file_name, stderr)


if __name__ == "__main__":
    args = parser.parse_args()

    for file_name, patch_config in PATCHES.items():
        file_path = os.path.join(RUST_SRC_DIR, file_name)

        if "replace_all" in patch_config:
            replace_all(file_path, patch_config["replace_all"])

        if "replace_all_null_ptr" in patch_config:
            formatted_replacements = []

            for replacement in patch_config["replace_all_null_ptr"]:
                formatted_replacements.append((replacement, "0"))

            replace_all(file_path, formatted_replacements)

        if "replace_all_null_ptr_cast" in patch_config:
            formatted_replacements = []

            for replacement in patch_config["replace_all_null_ptr_cast"]:
                if replacement.endswith("as_ptr()"):
                    formatted_replacements.append((replacement, "0 as *const _"))
                elif replacement.endswith("as_mut_ptr()"):
                    formatted_replacements.append((replacement, "0 as *mut _"))
                else:
                    assert False

            replace_all(file_path, formatted_replacements)

