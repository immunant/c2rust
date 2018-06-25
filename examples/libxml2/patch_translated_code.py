#! /usr/bin/env python3
# -*- coding: utf-8 -*-

from common import Config
from plumbum.cmd import echo, perl
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
PATCHES = {
    "src/threads.rs": {
        "replace_all": [
            ("unsafe \{ 1i32.wrapping_neg\(\) \}", "-1"),
        ],
    },
    "src/xmlunicode.rs": {
        "replace_all_null_ptr_cast": [
            "xmlUnicodeBlocks.as_ptr\(\)",
            "xmlUnicodeCats.as_mut_ptr\(\)",
            "xmlZS.as_ptr\(\)",
            "xmlSoL.as_ptr\(\)",
            "xmlSoS.as_ptr\(\)",
            "xmlSmL.as_ptr\(\)",
            "xmlSmS.as_ptr\(\)",
            "xmlSkS.as_ptr\(\)",
            "xmlScS.as_ptr\(\)",
            "xmlSL.as_ptr\(\)",
            "xmlSS.as_ptr\(\)",
            "xmlPsS.as_ptr\(\)",
            "xmlPoL.as_ptr\(\)",
            "xmlPoS.as_ptr\(\)",
            "xmlPeS.as_ptr\(\)",
            "xmlPdS.as_ptr\(\)",
            "xmlPL.as_ptr\(\)",
            "xmlPS.as_ptr\(\)",
            "xmlNoL.as_ptr\(\)",
            "xmlNoS.as_ptr\(\)",
            "xmlNdL.as_ptr\(\)",
            "xmlNdS.as_ptr\(\)",
            "xmlNL.as_ptr\(\)",
            "xmlNS.as_ptr\(\)",
            "xmlMnL.as_ptr\(\)",
            "xmlMnS.as_ptr\(\)",
            "xmlMcL.as_ptr\(\)",
            "xmlMcS.as_ptr\(\)",
            "xmlML.as_ptr\(\)",
            "xmlMS.as_ptr\(\)",
            "xmlLuL.as_ptr\(\)",
            "xmlLuS.as_ptr\(\)",
            "xmlLtS.as_ptr\(\)",
            "xmlLoL.as_ptr\(\)",
            "xmlLoS.as_ptr\(\)",
            "xmlLmS.as_ptr\(\)",
            "xmlLlL.as_ptr\(\)",
            "xmlLlS.as_ptr\(\)",
            "xmlLL.as_ptr\(\)",
            "xmlLS.as_ptr\(\)",
            "xmlCfL.as_ptr\(\)",
            "xmlCfS.as_ptr\(\)",
            "xmlCL.as_ptr\(\)",
            "xmlCS.as_ptr\(\)",
        ],
        "init_array": [
            "xmlUnicodeBlockTbl.table = xmlUnicodeBlocks.as_ptr();",
            "xmlUnicodeCatTbl.table = xmlUnicodeCats.as_mut_ptr();",
            "xmlZG.shortRange = xmlZS.as_ptr();",
            "xmlSoG.longRange = xmlSoL.as_ptr();",
            "xmlSoG.shortRange = xmlSoS.as_ptr();",
            "xmlSmG.longRange = xmlSmL.as_ptr();",
            "xmlSmG.shortRange = xmlSmS.as_ptr();",
            "xmlSkG.shortRange = xmlSkS.as_ptr();",
            "xmlScG.shortRange = xmlScS.as_ptr();",
            "xmlSG.longRange = xmlSL.as_ptr();",
            "xmlSG.shortRange = xmlSS.as_ptr();",
            "xmlPsG.shortRange = xmlPsS.as_ptr();",
            "xmlPoG.longRange = xmlPoL.as_ptr();",
            "xmlPoG.shortRange = xmlPoS.as_ptr();",
            "xmlPeG.shortRange = xmlPeS.as_ptr();",
            "xmlPdG.shortRange = xmlPdS.as_ptr();",
            "xmlPG.longRange = xmlPL.as_ptr();",
            "xmlPG.shortRange = xmlPS.as_ptr();",
            "xmlNoG.longRange = xmlNoL.as_ptr();",
            "xmlNoG.shortRange = xmlNoS.as_ptr();",
            "xmlNdG.longRange = xmlNdL.as_ptr();",
            "xmlNdG.shortRange = xmlNdS.as_ptr();",
            "xmlNG.longRange = xmlNL.as_ptr();",
            "xmlNG.shortRange = xmlNS.as_ptr();",
            "xmlMnG.longRange = xmlMnL.as_ptr();",
            "xmlMnG.shortRange = xmlMnS.as_ptr();",
            "xmlMcG.longRange = xmlMcL.as_ptr();",
            "xmlMcG.shortRange = xmlMcS.as_ptr();",
            "xmlMG.longRange = xmlML.as_ptr();",
            "xmlMG.shortRange = xmlMS.as_ptr();",
            "xmlLuG.longRange = xmlLuL.as_ptr();",
            "xmlLuG.shortRange = xmlLuS.as_ptr();",
            "xmlLtG.shortRange = xmlLtS.as_ptr();",
            "xmlLoG.longRange = xmlLoL.as_ptr();",
            "xmlLoG.shortRange = xmlLoS.as_ptr();",
            "xmlLmG.shortRange = xmlLmS.as_ptr();",
            "xmlLlG.longRange = xmlLlL.as_ptr();",
            "xmlLlG.shortRange = xmlLlS.as_ptr();",
            "xmlLG.longRange = xmlLL.as_ptr();",
            "xmlLG.shortRange = xmlLS.as_ptr();",
            "xmlCfG.longRange = xmlCfL.as_ptr();",
            "xmlCfG.shortRange = xmlCfS.as_ptr();",
            "xmlCG.longRange = xmlCL.as_ptr();",
            "xmlCG.shortRange = xmlCS.as_ptr();",
        ],
    },
    "src/chvalid.rs": {
        "replace_all_null_ptr_cast": [
            "xmlIsIdeographic_srng.as_ptr\(\)",
            "xmlIsExtender_srng.as_ptr\(\)",
            "xmlIsDigit_srng.as_ptr\(\)",
            "xmlIsCombining_srng.as_ptr\(\)",
            "xmlIsChar_lrng.as_ptr\(\)",
            "xmlIsChar_srng.as_ptr\(\)",
            "xmlIsBaseChar_srng.as_ptr\(\)",
        ],
        "init_array": [
            "xmlIsIdeographicGroup.shortRange = xmlIsIdeographic_srng.as_ptr();",
            "xmlIsExtenderGroup.shortRange = xmlIsExtender_srng.as_ptr();",
            "xmlIsDigitGroup.shortRange = xmlIsDigit_srng.as_ptr();",
            "xmlIsCombiningGroup.shortRange = xmlIsCombining_srng.as_ptr();",
            "xmlIsCharGroup.longRange = xmlIsChar_lrng.as_ptr();",
            "xmlIsCharGroup.shortRange = xmlIsChar_srng.as_ptr();",
            "xmlIsBaseCharGroup.shortRange = xmlIsBaseChar_srng.as_ptr();",
        ]
    },
    "src/HTMLparser.rs": {
        "replace_all_null_ptr": [
            "html_attrs.as_ptr\(\)",
            "html_inline.as_ptr\(\)",
            "ul_depr.as_ptr\(\)",
            " li_elt.as_ptr\(\)",
            "bgcolor_attr.as_ptr\(\)",
            "talign_attrs.as_ptr\(\)",
            "tr_contents.as_ptr\(\)",
            " i18n_attrs.as_ptr\(\)",
            "html_pcdata.as_ptr\(\)",
            "tr_elt.as_ptr\(\)",
            "th_td_depr.as_ptr\(\)",
            "th_td_attr.as_ptr\(\)",
            "html_flow.as_ptr\(\)",
            "rows_cols_attr.as_ptr\(\)",
            "textarea_attrs.as_ptr\(\)",
            "table_depr.as_ptr\(\)",
            "table_attrs.as_ptr\(\)",
            "table_contents.as_ptr\(\)",
            "type_attr.as_ptr\(\)",
            "style_attrs.as_ptr\(\)",
            "select_attrs.as_ptr\(\)",
            "select_content.as_ptr\(\)",
            "language_attr.as_ptr\(\)",
            "script_attrs.as_ptr\(\)",
            "quote_attrs.as_ptr\(\)",
            "width_attr.as_ptr\(\)",
            "pre_content.as_ptr\(\)",
            "name_attr.as_ptr\(\)",
            "param_attrs.as_ptr\(\)",
            "align_attr.as_ptr\(\)",
            "option_attrs.as_ptr\(\)",
            "label_attr.as_ptr\(\)",
            "optgroup_attrs.as_ptr\(\)",
            "option_elt.as_ptr\(\)",
            " ol_attrs.as_ptr\(\)",
            "object_depr.as_ptr\(\)",
            "object_attrs.as_ptr\(\)",
            "object_contents.as_ptr\(\)",
            "noframes_content.as_ptr\(\)",
            "content_attr.as_ptr\(\)",
            "meta_attrs.as_ptr\(\)",
            "compact_attrs.as_ptr\(\)",
            "blockli_elt.as_ptr\(\)",
            "map_contents.as_ptr\(\)",
            "target_attr.as_ptr\(\)",
            "link_attrs.as_ptr\(\)",
            "legend_attrs.as_ptr\(\)",
            "label_attrs.as_ptr\(\)",
            "prompt_attrs.as_ptr\(\)",
            "edit_attrs.as_ptr\(\)",
            " a_attrs.as_ptr\(\)",
            "inline_p.as_ptr\(\)",
            "flow_param.as_ptr\(\)",
            "applet_attrs.as_ptr\(\)",
            "area_attrs.as_ptr\(\)",
            "alt_attr.as_ptr\(\)",
            "href_attrs.as_ptr\(\)",
            "basefont_attrs.as_ptr\(\)",
            "core_i18n_attrs.as_ptr\(\)",
            "dir_attr.as_ptr\(\)",
            "body_contents.as_ptr\(\)",
            "body_attrs.as_ptr\(\)",
            "body_depr.as_ptr\(\)",
            "core_attrs.as_ptr\(\)",
            "clear_attrs.as_ptr\(\)",
            "button_attrs.as_ptr\(\)",
            "col_attrs.as_ptr\(\)",
            "col_elt.as_ptr\(\)",
            "dl_contents.as_ptr\(\)",
            "compact_attr.as_ptr\(\)",
            "embed_attrs.as_ptr\(\)",
            "fieldset_contents.as_ptr\(\)",
            " font_attrs.as_ptr\(\)",
            "form_contents.as_ptr\(\)",
            "form_attrs.as_ptr\(\)",
            "action_attr.as_ptr\(\)",
            " frame_attrs.as_ptr\(\)",
            "iframe_attrs.as_ptr\(\)",
            "frameset_contents.as_ptr\(\)",
            "frameset_attrs.as_ptr\(\)",
            "head_contents.as_ptr\(\)",
            "head_attrs.as_ptr\(\)",
            "hr_depr.as_ptr\(\)",
            "html_content.as_ptr\(\)",
            "version_attr.as_ptr\(\)",
            "img_attrs.as_ptr\(\)",
            "src_alt_attrs.as_ptr\(\)",
            "input_attrs.as_ptr\(\)",
        ],
        "init_array": [
            "html40ElementTable[1].attrs_opt = html_attrs.as_ptr() as *mut *const _;",
            "html40ElementTable[2].attrs_opt = html_attrs.as_ptr() as *mut *const _;",
            "html40ElementTable[3].attrs_opt = html_attrs.as_ptr() as *mut *const _;",
            "html40ElementTable[6].attrs_opt = html_attrs.as_ptr() as *mut *const _;",
            "html40ElementTable[10].attrs_opt = html_attrs.as_ptr() as *mut *const _;",
            "html40ElementTable[15].attrs_opt = html_attrs.as_ptr() as *mut *const _;",
            "html40ElementTable[16].attrs_depr = html_attrs.as_ptr() as *mut *const _; // attrs_depr",
            "html40ElementTable[17].attrs_opt = html_attrs.as_ptr() as *mut *const _;",
            "html40ElementTable[18].attrs_opt = html_attrs.as_ptr() as *mut *const _;",
            "html40ElementTable[21].attrs_opt = html_attrs.as_ptr() as *mut *const _;",
            "html40ElementTable[23].attrs_opt = html_attrs.as_ptr() as *mut *const _;",
            "html40ElementTable[25].attrs_opt = html_attrs.as_ptr() as *mut *const _;",
            "html40ElementTable[26].attrs_opt = html_attrs.as_ptr() as *mut *const _;",
            "html40ElementTable[27].attrs_opt = html_attrs.as_ptr() as *mut *const _;",
            "html40ElementTable[28].attrs_opt = html_attrs.as_ptr() as *mut *const _;",
            "html40ElementTable[30].attrs_opt = html_attrs.as_ptr() as *mut *const _;",
            "html40ElementTable[35].attrs_opt = html_attrs.as_ptr() as *mut *const _;",
            "html40ElementTable[36].attrs_opt = html_attrs.as_ptr() as *mut *const _;",
            "html40ElementTable[37].attrs_opt = html_attrs.as_ptr() as *mut *const _;",
            "html40ElementTable[38].attrs_opt = html_attrs.as_ptr() as *mut *const _;",
            "html40ElementTable[39].attrs_opt = html_attrs.as_ptr() as *mut *const _;",
            "html40ElementTable[40].attrs_opt = html_attrs.as_ptr() as *mut *const _;",
            "html40ElementTable[42].attrs_opt = html_attrs.as_ptr() as *mut *const _;",
            "html40ElementTable[44].attrs_opt = html_attrs.as_ptr() as *mut *const _;",
            "html40ElementTable[50].attrs_opt = html_attrs.as_ptr() as *mut *const _;",
            "html40ElementTable[53].attrs_opt = html_attrs.as_ptr() as *mut *const _;",
            "html40ElementTable[55].attrs_opt = html_attrs.as_ptr() as *mut *const _;",
            "html40ElementTable[58].attrs_opt = html_attrs.as_ptr() as *mut *const _;",
            "html40ElementTable[59].attrs_opt = html_attrs.as_ptr() as *mut *const _;",
            "html40ElementTable[61].attrs_opt = html_attrs.as_ptr() as *mut *const _;",
            "html40ElementTable[64].attrs_opt = html_attrs.as_ptr() as *mut *const _;",
            "html40ElementTable[66].attrs_opt = html_attrs.as_ptr() as *mut *const _;",
            "html40ElementTable[68].attrs_depr = html_attrs.as_ptr() as *mut *const _; // attrs_depr",
            "html40ElementTable[69].attrs_opt = html_attrs.as_ptr() as *mut *const _;",
            "html40ElementTable[72].attrs_opt = html_attrs.as_ptr() as *mut *const _;",
            "html40ElementTable[73].attrs_opt = html_attrs.as_ptr() as *mut *const _;",
            "html40ElementTable[74].attrs_depr = html_attrs.as_ptr() as *mut *const _; // attrs_depr",
            "html40ElementTable[75].attrs_opt = html_attrs.as_ptr() as *mut *const _;",
            "html40ElementTable[77].attrs_opt = html_attrs.as_ptr() as *mut *const _;",
            "html40ElementTable[78].attrs_opt = html_attrs.as_ptr() as *mut *const _;",
            "html40ElementTable[88].attrs_opt = html_attrs.as_ptr() as *mut *const _;",
            "html40ElementTable[89].attrs_depr = html_attrs.as_ptr() as *mut *const _; // attrs_depr",
            "html40ElementTable[90].attrs_opt = html_attrs.as_ptr() as *mut *const _;",
            "html40ElementTable[91].attrs_opt = html_attrs.as_ptr() as *mut *const _;",

            "html40ElementTable[0].subelts = html_inline.as_ptr() as *mut *const _;",
            "html40ElementTable[1].subelts = html_inline.as_ptr() as *mut *const _;",
            "html40ElementTable[2].subelts = html_inline.as_ptr() as *mut *const _;",
            "html40ElementTable[6].subelts = html_inline.as_ptr() as *mut *const _;",
            "html40ElementTable[9].subelts = html_inline.as_ptr() as *mut *const _;",
            "html40ElementTable[10].subelts = html_inline.as_ptr() as *mut *const _;",
            "html40ElementTable[15].subelts = html_inline.as_ptr() as *mut *const _;",
            "html40ElementTable[17].subelts = html_inline.as_ptr() as *mut *const _;",
            "html40ElementTable[18].subelts = html_inline.as_ptr() as *mut *const _;",
            "html40ElementTable[23].subelts = html_inline.as_ptr() as *mut *const _;",
            "html40ElementTable[27].subelts = html_inline.as_ptr() as *mut *const _;",
            "html40ElementTable[28].subelts = html_inline.as_ptr() as *mut *const _;",
            "html40ElementTable[31].subelts = html_inline.as_ptr() as *mut *const _;",
            "html40ElementTable[35].subelts = html_inline.as_ptr() as *mut *const _;",
            "html40ElementTable[36].subelts = html_inline.as_ptr() as *mut *const _;",
            "html40ElementTable[37].subelts = html_inline.as_ptr() as *mut *const _;",
            "html40ElementTable[38].subelts = html_inline.as_ptr() as *mut *const _;",
            "html40ElementTable[39].subelts = html_inline.as_ptr() as *mut *const _;",
            "html40ElementTable[40].subelts = html_inline.as_ptr() as *mut *const _;",
            "html40ElementTable[44].subelts = html_inline.as_ptr() as *mut *const _;",
            "html40ElementTable[50].subelts = html_inline.as_ptr() as *mut *const _;",
            "html40ElementTable[51].subelts = html_inline.as_ptr() as *mut *const _;",
            "html40ElementTable[52].subelts = html_inline.as_ptr() as *mut *const _;",
            "html40ElementTable[64].subelts = html_inline.as_ptr() as *mut *const _;",
            "html40ElementTable[67].subelts = html_inline.as_ptr() as *mut *const _;",
            "html40ElementTable[68].subelts = html_inline.as_ptr() as *mut *const _;",
            "html40ElementTable[69].subelts = html_inline.as_ptr() as *mut *const _;",
            "html40ElementTable[72].subelts = html_inline.as_ptr() as *mut *const _;",
            "html40ElementTable[73].subelts = html_inline.as_ptr() as *mut *const _;",
            "html40ElementTable[74].subelts = html_inline.as_ptr() as *mut *const _;",
            "html40ElementTable[75].subelts = html_inline.as_ptr() as *mut *const _;",
            "html40ElementTable[77].subelts = html_inline.as_ptr() as *mut *const _;",
            "html40ElementTable[78].subelts = html_inline.as_ptr() as *mut *const _;",
            "html40ElementTable[88].subelts = html_inline.as_ptr() as *mut *const _;",
            "html40ElementTable[89].subelts = html_inline.as_ptr() as *mut *const _;",
            "html40ElementTable[91].subelts = html_inline.as_ptr() as *mut *const _;",

            "html40ElementTable[90].attrs_depr = ul_depr.as_ptr() as *mut *const _;",

            "html40ElementTable[24].subelts = blockli_elt.as_ptr() as *mut *const _;",
            "html40ElementTable[56].subelts = blockli_elt.as_ptr() as *mut *const _;",

            "html40ElementTable[61].subelts = li_elt.as_ptr() as *mut *const _;",
            "html40ElementTable[90].subelts = li_elt.as_ptr() as *mut *const _;",

            "html40ElementTable[87].attrs_depr = bgcolor_attr.as_ptr() as *mut *const _;",

            "html40ElementTable[80].attrs_opt = talign_attrs.as_ptr() as *mut *const _;",
            "html40ElementTable[83].attrs_opt = talign_attrs.as_ptr() as *mut *const _;",
            "html40ElementTable[85].attrs_opt = talign_attrs.as_ptr() as *mut *const _;",
            "html40ElementTable[87].attrs_opt = talign_attrs.as_ptr() as *mut *const _;",

            "html40ElementTable[87].subelts = tr_contents.as_ptr() as *mut *const _;",

            "html40ElementTable[9].attrs_opt = core_i18n_attrs.as_ptr() as *mut *const _;",

            "html40ElementTable[43].attrs_opt = i18n_attrs.as_ptr() as *mut *const _;",
            "html40ElementTable[86].attrs_opt = i18n_attrs.as_ptr() as *mut *const _;",

            "html40ElementTable[63].subelts = html_pcdata.as_ptr() as *mut *const _;",
            "html40ElementTable[70].subelts = html_pcdata.as_ptr() as *mut *const _;",
            "html40ElementTable[76].subelts = html_pcdata.as_ptr() as *mut *const _;",
            "html40ElementTable[82].subelts = html_pcdata.as_ptr() as *mut *const _;",
            "html40ElementTable[86].subelts = html_pcdata.as_ptr() as *mut *const _;",

            "html40ElementTable[80].subelts = tr_elt.as_ptr() as *mut *const _;",
            "html40ElementTable[83].subelts = tr_elt.as_ptr() as *mut *const _;",
            "html40ElementTable[85].subelts = tr_elt.as_ptr() as *mut *const _;",

            "html40ElementTable[81].attrs_depr = th_td_depr.as_ptr() as *mut *const _;",
            "html40ElementTable[84].attrs_depr = th_td_depr.as_ptr() as *mut *const _;",

            "html40ElementTable[81].attrs_opt = th_td_attr.as_ptr() as *mut *const _;",
            "html40ElementTable[84].attrs_opt = th_td_attr.as_ptr() as *mut *const _;",

            "html40ElementTable[11].subelts = html_flow.as_ptr() as *mut *const _;",
            "html40ElementTable[14].subelts = html_flow.as_ptr() as *mut *const _;",
            "html40ElementTable[16].subelts = html_flow.as_ptr() as *mut *const _;",
            "html40ElementTable[21].subelts = html_flow.as_ptr() as *mut *const _;",
            "html40ElementTable[22].subelts = html_flow.as_ptr() as *mut *const _;",
            "html40ElementTable[25].subelts = html_flow.as_ptr() as *mut *const _;",
            "html40ElementTable[45].subelts = html_flow.as_ptr() as *mut *const _;",
            "html40ElementTable[48].subelts = html_flow.as_ptr() as *mut *const _;",
            "html40ElementTable[53].subelts = html_flow.as_ptr() as *mut *const _;",
            "html40ElementTable[59].subelts = html_flow.as_ptr() as *mut *const _;",
            "html40ElementTable[81].subelts = html_flow.as_ptr() as *mut *const _;",
            "html40ElementTable[84].subelts = html_flow.as_ptr() as *mut *const _;",

            "html40ElementTable[82].attrs_req = rows_cols_attr.as_ptr() as *mut *const _;",

            "html40ElementTable[82].attrs_opt = textarea_attrs.as_ptr() as *mut *const _;",

            "html40ElementTable[79].attrs_depr = table_depr.as_ptr() as *mut *const _;",

            "html40ElementTable[79].attrs_opt = table_attrs.as_ptr() as *mut *const _;",

            "html40ElementTable[79].subelts = table_contents.as_ptr() as *mut *const _;",

            "html40ElementTable[70].attrs_req = type_attr.as_ptr() as *mut *const _;",
            "html40ElementTable[76].attrs_req = type_attr.as_ptr() as *mut *const _;",

            "html40ElementTable[76].attrs_opt = style_attrs.as_ptr() as *mut *const _;",

            "html40ElementTable[71].attrs_opt = select_attrs.as_ptr() as *mut *const _;",

            "html40ElementTable[71].subelts = select_content.as_ptr() as *mut *const _;",

            "html40ElementTable[70].attrs_depr = language_attr.as_ptr() as *mut *const _;",

            "html40ElementTable[70].attrs_opt = script_attrs.as_ptr() as *mut *const _;",

            "html40ElementTable[11].attrs_opt = quote_attrs.as_ptr() as *mut *const _;",
            "html40ElementTable[67].attrs_opt = quote_attrs.as_ptr() as *mut *const _;",

            "html40ElementTable[66].attrs_depr = width_attr.as_ptr() as *mut *const _;",

            "html40ElementTable[66].subelts = pre_content.as_ptr() as *mut *const _;",

            "html40ElementTable[55].attrs_req = name_attr.as_ptr() as *mut *const _;",
            "html40ElementTable[65].attrs_req = name_attr.as_ptr() as *mut *const _;",

            "html40ElementTable[65].attrs_opt = param_attrs.as_ptr() as *mut *const _;",

            "html40ElementTable[25].attrs_depr = align_attr.as_ptr() as *mut *const _;",
            "html40ElementTable[35].attrs_depr = align_attr.as_ptr() as *mut *const _;",
            "html40ElementTable[36].attrs_depr = align_attr.as_ptr() as *mut *const _;",
            "html40ElementTable[37].attrs_depr = align_attr.as_ptr() as *mut *const _;",
            "html40ElementTable[38].attrs_depr = align_attr.as_ptr() as *mut *const _;",
            "html40ElementTable[39].attrs_depr = align_attr.as_ptr() as *mut *const _;",
            "html40ElementTable[40].attrs_depr = align_attr.as_ptr() as *mut *const _;",
            "html40ElementTable[46].attrs_depr = align_attr.as_ptr() as *mut *const _;",
            "html40ElementTable[47].attrs_depr = align_attr.as_ptr() as *mut *const _;",
            "html40ElementTable[52].attrs_depr = align_attr.as_ptr() as *mut *const _;",
            "html40ElementTable[64].attrs_depr = align_attr.as_ptr() as *mut *const _;",

            "html40ElementTable[63].attrs_opt = option_attrs.as_ptr() as *mut *const _;",

            "html40ElementTable[62].attrs_req = label_attr.as_ptr() as *mut *const _;",

            "html40ElementTable[62].attrs_opt = optgroup_attrs.as_ptr() as *mut *const _;",

            "html40ElementTable[62].subelts = option_elt.as_ptr() as *mut *const _;",

            "html40ElementTable[19].attrs_opt = col_attrs.as_ptr() as *mut *const _;",
            "html40ElementTable[20].attrs_opt = col_attrs.as_ptr() as *mut *const _;",

            "html40ElementTable[61].attrs_depr = ol_attrs.as_ptr() as *mut *const _;",

            "html40ElementTable[60].attrs_depr = object_depr.as_ptr() as *mut *const _;",

            "html40ElementTable[60].attrs_opt = object_attrs.as_ptr() as *mut *const _;",

            "html40ElementTable[60].subelts = object_contents.as_ptr() as *mut *const _;",

            "html40ElementTable[58].subelts = noframes_content.as_ptr() as *mut *const _;",

            "html40ElementTable[57].attrs_req = content_attr.as_ptr() as *mut *const _;",

            "html40ElementTable[57].attrs_opt = meta_attrs.as_ptr() as *mut *const _;",

            "html40ElementTable[24].attrs_depr = compact_attrs.as_ptr() as *mut *const _;",
            "html40ElementTable[56].attrs_depr = compact_attrs.as_ptr() as *mut *const _;",

            "html40ElementTable[55].subelts = map_contents.as_ptr() as *mut *const _;",

            "html40ElementTable[0].attrs_depr = target_attr.as_ptr() as *mut *const _;",
            "html40ElementTable[5].attrs_depr = target_attr.as_ptr() as *mut *const _;",
            "html40ElementTable[7].attrs_depr = target_attr.as_ptr() as *mut *const _;",
            "html40ElementTable[32].attrs_depr = target_attr.as_ptr() as *mut *const _;",
            "html40ElementTable[54].attrs_depr = target_attr.as_ptr() as *mut *const _;",

            "html40ElementTable[54].attrs_opt = link_attrs.as_ptr() as *mut *const _;",

            "html40ElementTable[52].attrs_opt = legend_attrs.as_ptr() as *mut *const _;",

            "html40ElementTable[51].attrs_opt = label_attrs.as_ptr() as *mut *const _;",

            "html40ElementTable[49].attrs_depr = prompt_attrs.as_ptr() as *mut *const _;",

            "html40ElementTable[22].attrs_opt = edit_attrs.as_ptr() as *mut *const _;",
            "html40ElementTable[48].attrs_opt = edit_attrs.as_ptr() as *mut *const _;",

            "html40ElementTable[0].attrs_opt = a_attrs.as_ptr() as *mut *const _;",

            "html40ElementTable[5].attrs_opt = area_attrs.as_ptr() as *mut *const _;",

            "html40ElementTable[3].subelts = inline_p.as_ptr() as *mut *const _;",

            "html40ElementTable[4].subelts = flow_param.as_ptr() as *mut *const _;",

            "html40ElementTable[4].attrs_depr = applet_attrs.as_ptr() as *mut *const _;",

            "html40ElementTable[5].attrs_req = alt_attr.as_ptr() as *mut *const _;",

            "html40ElementTable[7].attrs_req = href_attrs.as_ptr() as *mut *const _;",

            "html40ElementTable[8].attrs_depr = basefont_attrs.as_ptr() as *mut *const _;",

            "html40ElementTable[9].attrs_req = dir_attr.as_ptr() as *mut *const _;",

            "html40ElementTable[12].subelts = body_contents.as_ptr() as *mut *const _;",

            "html40ElementTable[12].attrs_opt = body_attrs.as_ptr() as *mut *const _;",

            "html40ElementTable[12].attrs_depr = body_depr.as_ptr() as *mut *const _;",

            "html40ElementTable[13].attrs_opt = core_attrs.as_ptr() as *mut *const _;",

            "html40ElementTable[13].attrs_depr = clear_attrs.as_ptr() as *mut *const _;",

            "html40ElementTable[14].attrs_opt = button_attrs.as_ptr() as *mut *const _;",

            "html40ElementTable[20].subelts = col_elt.as_ptr() as *mut *const _;",

            "html40ElementTable[26].subelts = dl_contents.as_ptr() as *mut *const _;",

            "html40ElementTable[26].attrs_depr = compact_attr.as_ptr() as *mut *const _;",

            "html40ElementTable[29].attrs_opt = embed_attrs.as_ptr() as *mut *const _;",

            "html40ElementTable[30].subelts = fieldset_contents.as_ptr() as *mut *const _;",

            "html40ElementTable[31].attrs_depr = font_attrs.as_ptr() as *mut *const _;",

            "html40ElementTable[32].subelts = form_contents.as_ptr() as *mut *const _;",

            "html40ElementTable[32].attrs_opt = form_attrs.as_ptr() as *mut *const _;",

            "html40ElementTable[32].attrs_req = action_attr.as_ptr() as *mut *const _;",

            "html40ElementTable[33].attrs_depr =  frame_attrs.as_ptr() as *mut *const _;",

            "html40ElementTable[45].attrs_depr = iframe_attrs.as_ptr() as *mut *const _;",

            "html40ElementTable[34].subelts = frameset_contents.as_ptr() as *mut *const _;",

            "html40ElementTable[34].attrs_depr = frameset_attrs.as_ptr() as *mut *const _;",

            "html40ElementTable[41].subelts = head_contents.as_ptr() as *mut *const _;",

            "html40ElementTable[41].attrs_opt = head_attrs.as_ptr() as *mut *const _;",

            "html40ElementTable[42].attrs_depr = hr_depr.as_ptr() as *mut *const _;",

            "html40ElementTable[43].subelts = html_content.as_ptr() as *mut *const _;",

            "html40ElementTable[43].attrs_depr = version_attr.as_ptr() as *mut *const _;",

            "html40ElementTable[46].attrs_opt = img_attrs.as_ptr() as *mut *const _;",

            "html40ElementTable[46].attrs_req = src_alt_attrs.as_ptr() as *mut *const _;",

            "html40ElementTable[47].attrs_opt = input_attrs.as_ptr() as *mut *const _;",
        ],
    },
    "examples/xmllint.rs": {
        "replace_all": [
            # Sed doesn't seem to like it when I add a \n between the externs:
            ("extern crate libc;", "extern crate libc;\nextern crate libxml2_rs;"),
        ],
    },
    "examples/runtest.rs": {
        "replace_all": [
            ("extern crate libc;", "#![feature(used)]\nextern crate libc;\nextern crate libxml2_rs;"),
            ("=\n\s*unsafe \{\n\s*\(::std::mem::size_of::<\[xmlThreadParams; 7]>\(\) as\n\s*libc::c_ulong\).wrapping_div\(::std::mem::size_of::<xmlThreadParams>\(\)\n\s*as libc::c_ulong\) as libc::c_uint\n\s*\};", "= 7;")
        ],
        "init_array": [
            "num_threads = (::std::mem::size_of::<[xmlThreadParams; 7]>() as libc::c_ulong).wrapping_div(::std::mem::size_of::<xmlThreadParams>() as libc::c_ulong) as libc::c_uint;"
        ]
    },
    "examples/testC14N.rs": {
        "replace_all": [
            ("extern crate libc;", "extern crate libc;\nextern crate libxml2_rs;"),
        ],
    },
    "examples/testHTML.rs": {
        "replace_all": [
            ("extern crate libc;", "extern crate libc;\nextern crate libxml2_rs;"),
        ],
    },
    "examples/testReader.rs": {
        "replace_all": [
            ("extern crate libc;", "extern crate libc;\nextern crate libxml2_rs;"),
        ],
    },
    "examples/testRelax.rs": {
        "replace_all": [
            ("extern crate libc;", "extern crate libc;\nextern crate libxml2_rs;"),
        ],
    },
    "examples/testThreads.rs": {
        "replace_all": [
            ("extern crate libc;", "extern crate libc;\nextern crate libxml2_rs;"),
        ],
    },
    "examples/testapi.rs": {
        "replace_all": [
            ("extern crate libc;", "extern crate libc;\nextern crate libxml2_rs;"),
        ],
    },
    "examples/testchar.rs": {
        "replace_all": [
            ("extern crate libc;", "extern crate libc;\nextern crate libxml2_rs;"),
        ],
    },
    "examples/testlimits.rs": {
        "replace_all": [
            ("extern crate libc;", "extern crate libc;\nextern crate libxml2_rs;"),
        ],
    },
    "examples/testrecurse.rs": {
        "replace_all": [
            ("extern crate libc;", "extern crate libc;\nextern crate libxml2_rs;"),
        ],
    },
    "examples/testSAX.rs": {
        "replace_all": [
            ("extern crate libc;", "extern crate libc;\nextern crate libxml2_rs;"),
        ],
    },
    "examples/testURI.rs": {
        "replace_all": [
            ("extern crate libc;", "extern crate libc;\nextern crate libxml2_rs;"),
        ],
    },
    "examples/testAutomata.rs": {
        "replace_all": [
            ("extern crate libc;", "extern crate libc;\nextern crate libxml2_rs;"),
        ],
    },
    "examples/testdict.rs": {
        "replace_all": [
            ("extern crate libc;", "extern crate libc;\nextern crate libxml2_rs;"),
        ],
    },
    "examples/testModule.rs": {
        "replace_all": [
            ("extern crate libc;", "extern crate libc;\nextern crate libxml2_rs;"),
        ],
    },
    "examples/testRegexp.rs": {
        "replace_all": [
            ("extern crate libc;", "extern crate libc;\nextern crate libxml2_rs;"),
        ],
    },
    "examples/testSchemas.rs": {
        "replace_all": [
            ("extern crate libc;", "extern crate libc;\nextern crate libxml2_rs;"),
        ],
    },
    "examples/testXPath.rs": {
        "replace_all": [
            ("extern crate libc;", "extern crate libc;\nextern crate libxml2_rs;"),
        ],
    },
}
INIT_ARRAY = """
extern "C" fn run_static_initializers() {{
    unsafe {{
{}
    }}
}}

#[cfg_attr(target_os = "linux", link_section = ".init_array")]
#[cfg_attr(target_os = "macos", link_section = "__DATA,__mod_init_func")]
#[used]
static INIT_ARRAY: [extern "C" fn(); 1] = [run_static_initializers];

#[cfg(not(any(target_os = "macos", target_os = "linux")))]
compile_error!("Your target OS does not have support for static initialization at the moment");"""

# TODO: Better error handling
def replace_all(file_path: str, replacements: Iterable[Tuple[str, str]]) -> None:
    perl_args = ["-000", "-pi"]

    for replace_from, replace_to in replacements:
        perl_args.append("-e")
        perl_args.append("s/{}/{}/g;".format(replace_from, replace_to))

    perl_args.append(file_path)

    retcode, stdout, stderr = perl[perl_args].run()

    assert retcode != 1, "Failed to apply patch {}/replace_all:\n{}".format(file_name, stderr)

if __name__ == "__main__":
    args = parser.parse_args()

    for file_name, patch_config in PATCHES.items():
        file_path = os.path.join(RUST_ROOT_DIR, file_name)

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
                if replacement.endswith("as_ptr\(\)"):
                    formatted_replacements.append((replacement, "0 as *const _"))
                elif replacement.endswith("as_mut_ptr\(\)"):
                    formatted_replacements.append((replacement, "0 as *mut _"))
                else:
                    assert False, "Could not find supported replacement"

            replace_all(file_path, formatted_replacements)

        if "init_array" in patch_config:
            initializers = "".join(["        " + initializer + '\n' for initializer in patch_config["init_array"]])
            init_array = INIT_ARRAY.format(initializers)

            retcode, stdout, stderr = (echo[init_array] >> file_path).run()

            assert retcode != 1, "Failed to apply patch {}/init_array:\n{}".format(file_name, stderr)
