/***
 *
 * Module:      tl/c/versions.c
 *
 * Copyright (c) 2001 The Thinlisp Group All Rights Reserved.
 *
 * Description: Translation of tl/lisp/versions.lisp.
 *    by ThinLisp http://www.thinlisp.org
 *
 */

#include "tl.h"
#include "versions.h"


static const Str_5 str_const
  = { 7, 2, 2, "TL" };

Obj tl_user_package = (Obj)(&Unbound);

static const Str_9 str_const_1
  = { 7, 7, 7, "TL-USER" };

static const Str_5 str_const_2
  = { 7, 3, 3, "NIL" };

/* Translated from NORMALIZE-SYSTEM-NAME(SYMBOL) = SYMBOL */

Obj normalize_system_name (Obj symbol)
{
  Obj tl_package_name;
  unsigned char *g;
  unsigned char *arg_temp;
  sint32 arg_temp_1;
  Obj temp;

  if (((Sym *)symbol)->symbol_package!=GET_GLOBAL(tl_user_package)) {
    if (symbol!=NULL) 
      g = (((Str *)(((Sym *)symbol)->symbol_name))->body);
    else 
      g = (((Str *)(&str_const_2))->body);      /* "NIL" */
    arg_temp = g;
    arg_temp_1 = sxhash_string(g);
    tl_package_name = intern_string_in_package(arg_temp,arg_temp_1,find_package_or_error_1(
        GET_GLOBAL(tl_user_package)));
  }
  else 
    tl_package_name = symbol;
  temp = get(tl_package_name,(Obj)(tl_versions_symbols+0),  /* NICKNAMES-TO */
      (Obj)NULL);
  if (temp==NULL) 
    temp = tl_package_name;
  return temp;
}

/* Translated from NORMALIZE-MODULE-NAME(SYMBOL) = SYMBOL */

Obj normalize_module_name (Obj symbol)
{
  unsigned char *g;
  unsigned char *arg_temp;
  sint32 arg_temp_1;

  if (((Sym *)symbol)->symbol_package!=GET_GLOBAL(tl_user_package)) {
    if (symbol!=NULL) 
      g = (((Str *)(((Sym *)symbol)->symbol_name))->body);
    else 
      g = (((Str *)(&str_const_2))->body);      /* "NIL" */
    arg_temp = g;
    arg_temp_1 = sxhash_string(g);
    return intern_string_in_package(arg_temp,arg_temp_1,find_package_or_error_1(
        GET_GLOBAL(tl_user_package)));
  }
  else 
    return symbol;
}

/* Translated from SYSTEM-MODULES(SYMBOL) = LIST */

Obj system_modules (Obj system_name)
{
  return get(normalize_system_name(system_name),(Obj)(tl_boot_symbols+3),   /* SYSTEM-MODULES */
      (Obj)NULL);
}

/* Translated from SYSTEM-ALIAS(SYMBOL) = SYMBOL */

Obj system_alias (Obj system_name)
{
  Obj name, temp;

  name = normalize_system_name(system_name);
  temp = get(name,(Obj)(tl_versions_symbols+1),(Obj)NULL);  /* ALIAS */
  if (temp==NULL) 
    temp = name;
  return temp;
}

/* Translated from SYSTEM-NICKNAMES(SYMBOL) = LIST */

Obj system_nicknames (Obj system_name)
{
  return get(normalize_system_name(system_name),(Obj)(tl_boot_symbols+1),   /* SYSTEM-NICKNAMES */
      (Obj)NULL);
}

/* Translated from SYSTEM-USED-SYSTEMS(SYMBOL) = LIST */

Obj system_used_systems (Obj system_name)
{
  return get(normalize_system_name(system_name),(Obj)(tl_boot_symbols+2),   /* SYSTEM-USED-SYSTEMS */
      (Obj)NULL);
}

Obj collected_systems = (Obj)(&Unbound);

/* Translated from SYSTEM-ALL-USED-SYSTEMS(SYMBOL) = LIST */

Obj system_all_used_systems (Obj system_name)
{
  Obj name, cached_listP, temp, temp_1;
  Thread_state *ts;
  Obj temp_2, new_collected_systems, temp_3;
  Thread_state *ts_1;

  name = normalize_system_name(system_name);
  cached_listP = get(name,(Obj)(tl_versions_symbols+2),     /* SYSTEM-ALL-USED-SYSTEMS */
      (Obj)NULL);
  temp = cached_listP;
  if (temp==NULL) {
    ts = THREAD_STATE;
    temp_1 = BOXFIX(0);
    bind_global(&current_region,ts,temp_1);
    new_collected_systems = (Obj)NULL;
    ts_1 = THREAD_STATE;
    bind_global(&collected_systems,ts_1,new_collected_systems);
    collect_all_used_systems(name);
    SET_GLOBAL(collected_systems,nreverse(GET_GLOBAL(collected_systems)));
    set_get(name,(Obj)(tl_versions_symbols+2),  /* SYSTEM-ALL-USED-SYSTEMS */
        GET_GLOBAL(collected_systems));
    temp_3 = GET_GLOBAL(collected_systems);
    unbind_global(&collected_systems,ts_1);
    temp_2 = temp_3;
    unbind_global(&current_region,ts);
    temp = temp_2;
  }
  return temp;
}

/* Translated from COLLECT-ALL-USED-SYSTEMS(T) = VOID */

void collect_all_used_systems (Obj name)
{
  Obj sub_system, tl_loop_list_, if_result_temp;

  sub_system = (Obj)NULL;
  tl_loop_list_ = get(name,(Obj)(tl_boot_symbols+2),    /* SYSTEM-USED-SYSTEMS */
      (Obj)NULL);
  while (tl_loop_list_!=NULL) {
    sub_system = CAR(tl_loop_list_);
    tl_loop_list_ = CDR(tl_loop_list_);
    collect_all_used_systems(sub_system);
  }

  if (memq(name,GET_GLOBAL(collected_systems))!=NULL) 
    if_result_temp = GET_GLOBAL(collected_systems);
  else 
    if_result_temp = alloc_cons(name,GET_GLOBAL(collected_systems),-1);
  SET_GLOBAL(collected_systems,if_result_temp);
  return;
}

Obj i386_code = BOXFIX(1);

Obj dos_code = BOXFIX(2);

Obj aviion_code = BOXFIX(3);

Obj sgi_code = BOXFIX(4);

Obj sequent_code = BOXFIX(5);

Obj next_code = BOXFIX(6);

Obj decstation_code = BOXFIX(7);

Obj masscomp_code = BOXFIX(8);

Obj hp9000s300_code = BOXFIX(9);

Obj hp9000s400_code = BOXFIX(10);

Obj hp9000s700_code = BOXFIX(11);

Obj hp9000s800_code = BOXFIX(12);

Obj rs6000_code = BOXFIX(13);

Obj sun3_code = BOXFIX(14);

Obj sun4_code = BOXFIX(15);

Obj sparcsol_code = BOXFIX(16);

Obj alphavms_code = BOXFIX(17);

Obj motorola_code = BOXFIX(18);

Obj vms_code = BOXFIX(19);

Obj stratus_code = BOXFIX(20);

Obj harris_code = BOXFIX(21);

Obj nec_code = BOXFIX(22);

Obj alphaosf_code = BOXFIX(23);

Obj alphant_code = BOXFIX(24);

Obj intelnt_code = BOXFIX(25);

Obj ncr_code = BOXFIX(26);

Obj windows95_code = BOXFIX(27);

Obj freebsd_code = BOXFIX(28);

Obj linux386_code = BOXFIX(29);

Obj macosx_code = BOXFIX(30);

Obj g2_operating_system = (Obj)(&Unbound);

static const Str_45 str_const_3
  = { 7, 43, 43, "Unknown platform code ~a, assuming UNIX o/s" };

Obj g2_machine_type = (Obj)(&Unbound);

Obj machine_model_var = (Obj)(&Unbound);

static const Str_21 str_const_4
  = { 7, 20, 20, " Data General AViiON" };

static const Str_17 str_const_5
  = { 7, 15, 15, " Motorola 88000" };

static const Str_9 str_const_6
  = { 7, 5, 5, " NeXT" };

static const Str_21 str_const_7
  = { 7, 17, 17, " Silicon Graphics" };

static const Str_9 str_const_8
  = { 7, 8, 8, " Sequent" };

static const Str_13 str_const_9
  = { 7, 11, 11, " DECstation" };

static const Str_21 str_const_10
  = { 7, 17, 17, " Concurrent 6000s" };

static const Str_13 str_const_11
  = { 7, 12, 12, " HP9000/300s" };

static const Str_13 str_const_12
  = { 7, 12, 12, " HP9000/400s" };

static const Str_13 str_const_13
  = { 7, 12, 12, " HP9000/700s" };

static const Str_13 str_const_14
  = { 7, 12, 12, " HP9000/800s" };

static const Str_9 str_const_15
  = { 7, 5, 5, " \'386" };

static const Str_21 str_const_16
  = { 7, 17, 17, " IBM POWERstation" };

static const Str_9 str_const_17
  = { 7, 6, 6, " Sun 3" };

static const Str_9 str_const_18
  = { 7, 8, 8, " Vax VMS" };

static const Str_25 str_const_19
  = { 7, 22, 22, " DEC Alpha AXP/OpenVMS" };

static const Str_21 str_const_20
  = { 7, 19, 19, " Sun 4/SPARCstation" };

static const Str_29 str_const_21
  = { 7, 27, 27, " Sun 4/SPARCstation Solaris" };

static const Str_21 str_const_22
  = { 7, 18, 18, " Harris Night Hawk" };

static const Str_13 str_const_23
  = { 7, 12, 12, " Stratus FTX" };

static const Str_21 str_const_24
  = { 7, 19, 19, " NEC EWS4800 Series" };

static const Str_17 str_const_25
  = { 7, 14, 14, " DEC OSF/1 AXP" };

static const Str_13 str_const_26
  = { 7, 9, 9, " Alpha NT" };

static const Str_13 str_const_27
  = { 7, 9, 9, " Intel NT" };

static const Str_17 str_const_28
  = { 7, 14, 14, "MS Windows 3.1" };

static const Str_13 str_const_29
  = { 7, 12, 12, " NCR 386/486" };

static const Str_13 str_const_30
  = { 7, 11, 11, " Windows 95" };

static const Str_17 str_const_31
  = { 7, 15, 15, " PowerPC MacOSX" };

static const Str_21 str_const_32
  = { 7, 18, 18, " Experimental Port" };

/* Translated from MACHINE-MODEL() = * */

Obj machine_model (void)
{
  Obj temp, g, if_result_temp, temp_1;

  if (GET_GLOBAL(machine_model_var)!=NULL) {
    temp = GET_GLOBAL(machine_model_var);
    Values_count = 1;
    return temp;
  }
  else {
    g = GET_GLOBAL(g2_machine_type);
    if (g==(Obj)(tl_versions_symbols+8))        /* AVIION */
      if_result_temp = (Obj)(&str_const_4);     /* " Data General AViiON" */
    else if (g==(Obj)(tl_versions_symbols+23))      /* MOTOROLA */
      if_result_temp = (Obj)(&str_const_5);     /* " Motorola 88000" */
    else if (g==(Obj)(tl_versions_symbols+11))      /* NEXT */
      if_result_temp = (Obj)(&str_const_6);     /* " NeXT" */
    else if (g==(Obj)(tl_versions_symbols+9))   /* SGI */
      if_result_temp = (Obj)(&str_const_7);     /* " Silicon Graphics" */
    else if (g==(Obj)(tl_versions_symbols+10))      /* SEQUENT */
      if_result_temp = (Obj)(&str_const_8);     /* " Sequent" */
    else if (g==(Obj)(tl_versions_symbols+12))      /* DECSTATION */
      if_result_temp = (Obj)(&str_const_9);     /* " DECstation" */
    else if (g==(Obj)(tl_versions_symbols+13))      /* MASSCOMP */
      if_result_temp = (Obj)(&str_const_10);    /* " Concurrent 6000s" */
    else if (g==(Obj)(tl_versions_symbols+14))      /* HP9000S300 */
      if_result_temp = (Obj)(&str_const_11);    /* " HP9000/300s" */
    else if (g==(Obj)(tl_versions_symbols+15))      /* HP9000S400 */
      if_result_temp = (Obj)(&str_const_12);    /* " HP9000/400s" */
    else if (g==(Obj)(tl_versions_symbols+16))      /* HP9000S700 */
      if_result_temp = (Obj)(&str_const_13);    /* " HP9000/700s" */
    else if (g==(Obj)(tl_versions_symbols+17))      /* HP9000S800 */
      if_result_temp = (Obj)(&str_const_14);    /* " HP9000/800s" */
    else if ((g==(Obj)(tl_versions_symbols+7)) || (g==(Obj)(tl_versions_symbols     /* I386 */
        +36)))                                  /* COMPAQ */
      if_result_temp = (Obj)(&str_const_15);    /* " '386" */
    else if (g==(Obj)(tl_versions_symbols+18))      /* RS6000 */
      if_result_temp = (Obj)(&str_const_16);    /* " IBM POWERstation" */
    else if (g==(Obj)(tl_versions_symbols+19))      /* SUN3 */
      if_result_temp = (Obj)(&str_const_17);    /* " Sun 3" */
    else if (g==(Obj)(tl_versions_symbols+5))   /* VMS */
      if_result_temp = (Obj)(&str_const_18);    /* " Vax VMS" */
    else if (g==(Obj)(tl_versions_symbols+22))      /* ALPHAVMS */
      if_result_temp = (Obj)(&str_const_19);    /* " DEC Alpha AXP/OpenVMS" */
    else if (g==(Obj)(tl_versions_symbols+20))      /* SUN4 */
      if_result_temp = (Obj)(&str_const_20);    /* " Sun 4/SPARCstation" */
    else if (g==(Obj)(tl_versions_symbols+21))      /* SPARCSOL */
      if_result_temp = (Obj)(&str_const_21);    /* " Sun 4/SPARCstation Solaris" */
    else if (g==(Obj)(tl_versions_symbols+25))      /* HARRIS */
      if_result_temp = (Obj)(&str_const_22);    /* " Harris Night Hawk" */
    else if (g==(Obj)(tl_versions_symbols+24))      /* STRATUS */
      if_result_temp = (Obj)(&str_const_23);    /* " Stratus FTX" */
    else if (g==(Obj)(tl_versions_symbols+26))      /* NEC */
      if_result_temp = (Obj)(&str_const_24);    /* " NEC EWS4800 Series" */
    else if (g==(Obj)(tl_versions_symbols+27))      /* ALPHAOSF */
      if_result_temp = (Obj)(&str_const_25);    /* " DEC OSF/1 AXP" */
    else if (g==(Obj)(tl_versions_symbols+28))      /* ALPHANT */
      if_result_temp = (Obj)(&str_const_26);    /* " Alpha NT" */
    else if (g==(Obj)(tl_versions_symbols+29))      /* INTELNT */
      if_result_temp = (Obj)(&str_const_27);    /* " Intel NT" */
    else if (g==(Obj)(tl_versions_symbols+4))   /* DOS */
      if_result_temp = (Obj)(&str_const_28);    /* "MS Windows 3.1" */
    else if (g==(Obj)(tl_versions_symbols+30))      /* NCR */
      if_result_temp = (Obj)(&str_const_29);    /* " NCR 386/486" */
    else if (g==(Obj)(tl_versions_symbols+31))      /* WINDOWS95 */
      if_result_temp = (Obj)(&str_const_30);    /* " Windows 95" */
    else if (g==(Obj)(tl_versions_symbols+34))      /* MACOSX */
      if_result_temp = (Obj)(&str_const_31);    /* " PowerPC MacOSX" */
    else 
      if_result_temp = (Obj)(&str_const_32);    /* " Experimental Port" */
    temp_1 = SET_GLOBAL(machine_model_var,if_result_temp);
    Values_count = 1;
    return temp_1;
  }
}

static const Str_9 str_const_33
  = { 7, 7, 7, "KEYWORD" };

static const Str_13 str_const_34
  = { 7, 12, 12, "NICKNAMES-TO" };

static const Str_9 str_const_35
  = { 7, 5, 5, "ALIAS" };

static const Str_25 str_const_36
  = { 7, 23, 23, "SYSTEM-ALL-USED-SYSTEMS" };

static const Str_5 str_const_37
  = { 7, 4, 4, "UNIX" };

static const Str_5 str_const_38
  = { 7, 3, 3, "DOS" };

static const Str_5 str_const_39
  = { 7, 3, 3, "VMS" };

static const Str_9 str_const_40
  = { 7, 5, 5, "WIN32" };

static const Str_5 str_const_41
  = { 7, 4, 4, "I386" };

static const Str_9 str_const_42
  = { 7, 6, 6, "AVIION" };

static const Str_5 str_const_43
  = { 7, 3, 3, "SGI" };

static const Str_9 str_const_44
  = { 7, 7, 7, "SEQUENT" };

static const Str_5 str_const_45
  = { 7, 4, 4, "NEXT" };

static const Str_13 str_const_46
  = { 7, 10, 10, "DECSTATION" };

static const Str_9 str_const_47
  = { 7, 8, 8, "MASSCOMP" };

static const Str_13 str_const_48
  = { 7, 10, 10, "HP9000S300" };

static const Str_13 str_const_49
  = { 7, 10, 10, "HP9000S400" };

static const Str_13 str_const_50
  = { 7, 10, 10, "HP9000S700" };

static const Str_13 str_const_51
  = { 7, 10, 10, "HP9000S800" };

static const Str_9 str_const_52
  = { 7, 6, 6, "RS6000" };

static const Str_5 str_const_53
  = { 7, 4, 4, "SUN3" };

static const Str_5 str_const_54
  = { 7, 4, 4, "SUN4" };

static const Str_9 str_const_55
  = { 7, 8, 8, "SPARCSOL" };

static const Str_9 str_const_56
  = { 7, 8, 8, "ALPHAVMS" };

static const Str_9 str_const_57
  = { 7, 8, 8, "MOTOROLA" };

static const Str_9 str_const_58
  = { 7, 7, 7, "STRATUS" };

static const Str_9 str_const_59
  = { 7, 6, 6, "HARRIS" };

static const Str_5 str_const_60
  = { 7, 3, 3, "NEC" };

static const Str_9 str_const_61
  = { 7, 8, 8, "ALPHAOSF" };

static const Str_9 str_const_62
  = { 7, 7, 7, "ALPHANT" };

static const Str_9 str_const_63
  = { 7, 7, 7, "INTELNT" };

static const Str_5 str_const_64
  = { 7, 3, 3, "NCR" };

static const Str_13 str_const_65
  = { 7, 9, 9, "WINDOWS95" };

static const Str_9 str_const_66
  = { 7, 7, 7, "FREEBSD" };

static const Str_9 str_const_67
  = { 7, 5, 5, "LINUX" };

static const Str_9 str_const_68
  = { 7, 6, 6, "MACOSX" };

static const Str_21 str_const_69
  = { 7, 17, 17, "EXPERIMENTAL-PORT" };

static const Str_9 str_const_70
  = { 7, 6, 6, "COMPAQ" };

Sym tl_versions_symbols[37];

/* Translated from SYMS-TL-VERSIONS() = VOID */

void syms_tl_versions (void)
{
  Obj cached_keyword_package, cached_tl_package;

  cached_keyword_package = find_package_1((Obj)(&str_const_33));    /* "KEYWORD" */
  init_symbol_into_package((Obj)(&(tl_versions_symbols[0])),(Obj)(&str_const_34),   /* "NICKNAMES-TO" */
      41336,cached_keyword_package);
  (tl_versions_symbols[0]).external = 1;
  (tl_versions_symbols[0]).symbol_value = (Obj)(&(tl_versions_symbols[0]));
  init_symbol_into_package((Obj)(&(tl_versions_symbols[1])),(Obj)(&str_const_35),   /* "ALIAS" */
      1925,cached_keyword_package);
  (tl_versions_symbols[1]).external = 1;
  (tl_versions_symbols[1]).symbol_value = (Obj)(&(tl_versions_symbols[1]));
  init_symbol_into_package((Obj)(&(tl_versions_symbols[2])),(Obj)(&str_const_36),   /* "SYSTEM-ALL-USED-SYSTEMS" */
      60613,cached_keyword_package);
  (tl_versions_symbols[2]).external = 1;
  (tl_versions_symbols[2]).symbol_value = (Obj)(&(tl_versions_symbols[2]));
  cached_tl_package = find_package_1((Obj)(&str_const));    /* "TL" */
  init_symbol_into_package((Obj)(&(tl_versions_symbols[3])),(Obj)(&str_const_37),   /* "UNIX" */
      858,cached_tl_package);
  init_symbol_into_package((Obj)(&(tl_versions_symbols[4])),(Obj)(&str_const_38),   /* "DOS" */
      477,cached_tl_package);
  init_symbol_into_package((Obj)(&(tl_versions_symbols[5])),(Obj)(&str_const_39),   /* "VMS" */
      401,cached_tl_package);
  init_symbol_into_package((Obj)(&(tl_versions_symbols[6])),(Obj)(&str_const_40),   /* "WIN32" */
      1620,cached_tl_package);
  init_symbol_into_package((Obj)(&(tl_versions_symbols[7])),(Obj)(&str_const_41),   /* "I386" */
      706,cached_tl_package);
  init_symbol_into_package((Obj)(&(tl_versions_symbols[8])),(Obj)(&str_const_42),   /* "AVIION" */
      3836,cached_tl_package);
  init_symbol_into_package((Obj)(&(tl_versions_symbols[9])),(Obj)(&str_const_43),   /* "SGI" */
      395,cached_tl_package);
  init_symbol_into_package((Obj)(&(tl_versions_symbols[10])),(Obj)(&str_const_44),  /* "SEQUENT" */
      6660,cached_tl_package);
  init_symbol_into_package((Obj)(&(tl_versions_symbols[11])),(Obj)(&str_const_45),  /* "NEXT" */
      896,cached_tl_package);
  init_symbol_into_package((Obj)(&(tl_versions_symbols[12])),(Obj)(&str_const_46),  /* "DECSTATION" */
      62852,cached_tl_package);
  init_symbol_into_package((Obj)(&(tl_versions_symbols[13])),(Obj)(&str_const_47),  /* "MASSCOMP" */
      14974,cached_tl_package);
  init_symbol_into_package((Obj)(&(tl_versions_symbols[14])),(Obj)(&str_const_48),  /* "HP9000S300" */
      55172,cached_tl_package);
  init_symbol_into_package((Obj)(&(tl_versions_symbols[15])),(Obj)(&str_const_49),  /* "HP9000S400" */
      55192,cached_tl_package);
  init_symbol_into_package((Obj)(&(tl_versions_symbols[16])),(Obj)(&str_const_50),  /* "HP9000S700" */
      55188,cached_tl_package);
  init_symbol_into_package((Obj)(&(tl_versions_symbols[17])),(Obj)(&str_const_51),  /* "HP9000S800" */
      55208,cached_tl_package);
  init_symbol_into_package((Obj)(&(tl_versions_symbols[18])),(Obj)(&str_const_52),  /* "RS6000" */
      3664,cached_tl_package);
  init_symbol_into_package((Obj)(&(tl_versions_symbols[19])),(Obj)(&str_const_53),  /* "SUN3" */
      867,cached_tl_package);
  init_symbol_into_package((Obj)(&(tl_versions_symbols[20])),(Obj)(&str_const_54),  /* "SUN4" */
      868,cached_tl_package);
  init_symbol_into_package((Obj)(&(tl_versions_symbols[21])),(Obj)(&str_const_55),  /* "SPARCSOL" */
      13062,cached_tl_package);
  init_symbol_into_package((Obj)(&(tl_versions_symbols[22])),(Obj)(&str_const_56),  /* "ALPHAVMS" */
      16025,cached_tl_package);
  init_symbol_into_package((Obj)(&(tl_versions_symbols[23])),(Obj)(&str_const_57),  /* "MOTOROLA" */
      14405,cached_tl_package);
  init_symbol_into_package((Obj)(&(tl_versions_symbols[24])),(Obj)(&str_const_58),  /* "STRATUS" */
      6337,cached_tl_package);
  init_symbol_into_package((Obj)(&(tl_versions_symbols[25])),(Obj)(&str_const_59),  /* "HARRIS" */
      3593,cached_tl_package);
  init_symbol_into_package((Obj)(&(tl_versions_symbols[26])),(Obj)(&str_const_60),  /* "NEC" */
      497,cached_tl_package);
  init_symbol_into_package((Obj)(&(tl_versions_symbols[27])),(Obj)(&str_const_61),  /* "ALPHAOSF" */
      16084,cached_tl_package);
  init_symbol_into_package((Obj)(&(tl_versions_symbols[28])),(Obj)(&str_const_62),  /* "ALPHANT" */
      8012,cached_tl_package);
  init_symbol_into_package((Obj)(&(tl_versions_symbols[29])),(Obj)(&str_const_63),  /* "INTELNT" */
      7440,cached_tl_package);
  init_symbol_into_package((Obj)(&(tl_versions_symbols[30])),(Obj)(&str_const_64),  /* "NCR" */
      492,cached_tl_package);
  init_symbol_into_package((Obj)(&(tl_versions_symbols[31])),(Obj)(&str_const_65),  /* "WINDOWS95" */
      28611,cached_tl_package);
  init_symbol_into_package((Obj)(&(tl_versions_symbols[32])),(Obj)(&str_const_66),  /* "FREEBSD" */
      7250,cached_tl_package);
  init_symbol_into_package((Obj)(&(tl_versions_symbols[33])),(Obj)(&str_const_67),  /* "LINUX" */
      1858,cached_tl_package);
  init_symbol_into_package((Obj)(&(tl_versions_symbols[34])),(Obj)(&str_const_68),  /* "MACOSX" */
      3690,cached_tl_package);
  init_symbol_into_package((Obj)(&(tl_versions_symbols[35])),(Obj)(&str_const_69),  /* "EXPERIMENTAL-PORT" */
      2607,cached_tl_package);
  init_symbol_into_package((Obj)(&(tl_versions_symbols[36])),(Obj)(&str_const_70),  /* "COMPAQ" */
      3947,cached_tl_package);
  return;
}


/* Translated from INIT-TL-VERSIONS() = VOID */

void init_tl_versions (void)
{
  sint32 platform_code;
  Obj if_result_temp;
  Obj temp_list[2];
  sint32 platform_code_1;
  Obj if_result_temp_1;

  SpackageS = find_package_1((Obj)(&str_const));    /* "TL" */
  if (tl_user_package==(Obj)(&Unbound)) 
    tl_user_package = find_package_1((Obj)(&str_const_1));  /* "TL-USER" */
  if (collected_systems==(Obj)(&Unbound)) 
    collected_systems = (Obj)NULL;
  if (g2_operating_system==(Obj)(&Unbound)) {
    platform_code = get_platform_code();
    if (platform_code==1) 
      if_result_temp = (Obj)(tl_versions_symbols+3);    /* UNIX */
    else if (platform_code==2) 
      if_result_temp = (Obj)(tl_versions_symbols+4);    /* DOS */
    else if (platform_code==3) 
      if_result_temp = (Obj)(tl_versions_symbols+3);    /* UNIX */
    else if (platform_code==4) 
      if_result_temp = (Obj)(tl_versions_symbols+3);    /* UNIX */
    else if (platform_code==5) 
      if_result_temp = (Obj)(tl_versions_symbols+3);    /* UNIX */
    else if (platform_code==6) 
      if_result_temp = (Obj)(tl_versions_symbols+3);    /* UNIX */
    else if (platform_code==7) 
      if_result_temp = (Obj)(tl_versions_symbols+3);    /* UNIX */
    else if (platform_code==8) 
      if_result_temp = (Obj)(tl_versions_symbols+3);    /* UNIX */
    else if (platform_code==9) 
      if_result_temp = (Obj)(tl_versions_symbols+3);    /* UNIX */
    else if (platform_code==10) 
      if_result_temp = (Obj)(tl_versions_symbols+3);    /* UNIX */
    else if (platform_code==11) 
      if_result_temp = (Obj)(tl_versions_symbols+3);    /* UNIX */
    else if (platform_code==12) 
      if_result_temp = (Obj)(tl_versions_symbols+3);    /* UNIX */
    else if (platform_code==13) 
      if_result_temp = (Obj)(tl_versions_symbols+3);    /* UNIX */
    else if (platform_code==14) 
      if_result_temp = (Obj)(tl_versions_symbols+3);    /* UNIX */
    else if (platform_code==15) 
      if_result_temp = (Obj)(tl_versions_symbols+3);    /* UNIX */
    else if (platform_code==16) 
      if_result_temp = (Obj)(tl_versions_symbols+3);    /* UNIX */
    else if (platform_code==17) 
      if_result_temp = (Obj)(tl_versions_symbols+5);    /* VMS */
    else if (platform_code==18) 
      if_result_temp = (Obj)(tl_versions_symbols+3);    /* UNIX */
    else if (platform_code==19) 
      if_result_temp = (Obj)(tl_versions_symbols+5);    /* VMS */
    else if (platform_code==20) 
      if_result_temp = (Obj)(tl_versions_symbols+3);    /* UNIX */
    else if (platform_code==21) 
      if_result_temp = (Obj)(tl_versions_symbols+3);    /* UNIX */
    else if (platform_code==22) 
      if_result_temp = (Obj)(tl_versions_symbols+3);    /* UNIX */
    else if (platform_code==23) 
      if_result_temp = (Obj)(tl_versions_symbols+3);    /* UNIX */
    else if (platform_code==24) 
      if_result_temp = (Obj)(tl_versions_symbols+6);    /* WIN32 */
    else if (platform_code==25) 
      if_result_temp = (Obj)(tl_versions_symbols+6);    /* WIN32 */
    else if (platform_code==26) 
      if_result_temp = (Obj)(tl_versions_symbols+3);    /* UNIX */
    else if (platform_code==27) 
      if_result_temp = (Obj)(tl_versions_symbols+6);    /* WIN32 */
    else if (platform_code==28) 
      if_result_temp = (Obj)(tl_versions_symbols+3);    /* UNIX */
    else if (platform_code==29) 
      if_result_temp = (Obj)(tl_versions_symbols+3);    /* UNIX */
    else if (platform_code==30) 
      if_result_temp = (Obj)(tl_versions_symbols+3);    /* UNIX */
    else {
      (temp_list[0]) = BOXFIX(platform_code);
      (temp_list[1]) = (Obj)NULL;
      format_function((Obj)(&T),((Str *)(&str_const_3))->body,  /* "Unknown platform code ~a, assuming UNIX o/s" */
          (Obj)(((uint32)temp_list)+2));
      if_result_temp = (Obj)(tl_versions_symbols+3);    /* UNIX */
    }
    g2_operating_system = if_result_temp;
  }
  if (g2_machine_type==(Obj)(&Unbound)) {
    platform_code_1 = get_platform_code();
    if (platform_code_1==1) 
      if_result_temp_1 = (Obj)(tl_versions_symbols+7);  /* I386 */
    else if (platform_code_1==2) 
      if_result_temp_1 = (Obj)(tl_versions_symbols+4);  /* DOS */
    else if (platform_code_1==3) 
      if_result_temp_1 = (Obj)(tl_versions_symbols+8);  /* AVIION */
    else if (platform_code_1==4) 
      if_result_temp_1 = (Obj)(tl_versions_symbols+9);  /* SGI */
    else if (platform_code_1==5) 
      if_result_temp_1 = (Obj)(tl_versions_symbols+10);     /* SEQUENT */
    else if (platform_code_1==6) 
      if_result_temp_1 = (Obj)(tl_versions_symbols+11);     /* NEXT */
    else if (platform_code_1==7) 
      if_result_temp_1 = (Obj)(tl_versions_symbols+12);     /* DECSTATION */
    else if (platform_code_1==8) 
      if_result_temp_1 = (Obj)(tl_versions_symbols+13);     /* MASSCOMP */
    else if (platform_code_1==9) 
      if_result_temp_1 = (Obj)(tl_versions_symbols+14);     /* HP9000S300 */
    else if (platform_code_1==10) 
      if_result_temp_1 = (Obj)(tl_versions_symbols+15);     /* HP9000S400 */
    else if (platform_code_1==11) 
      if_result_temp_1 = (Obj)(tl_versions_symbols+16);     /* HP9000S700 */
    else if (platform_code_1==12) 
      if_result_temp_1 = (Obj)(tl_versions_symbols+17);     /* HP9000S800 */
    else if (platform_code_1==13) 
      if_result_temp_1 = (Obj)(tl_versions_symbols+18);     /* RS6000 */
    else if (platform_code_1==14) 
      if_result_temp_1 = (Obj)(tl_versions_symbols+19);     /* SUN3 */
    else if (platform_code_1==15) 
      if_result_temp_1 = (Obj)(tl_versions_symbols+20);     /* SUN4 */
    else if (platform_code_1==16) 
      if_result_temp_1 = (Obj)(tl_versions_symbols+21);     /* SPARCSOL */
    else if (platform_code_1==17) 
      if_result_temp_1 = (Obj)(tl_versions_symbols+22);     /* ALPHAVMS */
    else if (platform_code_1==18) 
      if_result_temp_1 = (Obj)(tl_versions_symbols+23);     /* MOTOROLA */
    else if (platform_code_1==19) 
      if_result_temp_1 = (Obj)(tl_versions_symbols+5);  /* VMS */
    else if (platform_code_1==20) 
      if_result_temp_1 = (Obj)(tl_versions_symbols+24);     /* STRATUS */
    else if (platform_code_1==21) 
      if_result_temp_1 = (Obj)(tl_versions_symbols+25);     /* HARRIS */
    else if (platform_code_1==22) 
      if_result_temp_1 = (Obj)(tl_versions_symbols+26);     /* NEC */
    else if (platform_code_1==23) 
      if_result_temp_1 = (Obj)(tl_versions_symbols+27);     /* ALPHAOSF */
    else if (platform_code_1==24) 
      if_result_temp_1 = (Obj)(tl_versions_symbols+28);     /* ALPHANT */
    else if (platform_code_1==25) 
      if_result_temp_1 = (Obj)(tl_versions_symbols+29);     /* INTELNT */
    else if (platform_code_1==26) 
      if_result_temp_1 = (Obj)(tl_versions_symbols+30);     /* NCR */
    else if (platform_code_1==27) 
      if_result_temp_1 = (Obj)(tl_versions_symbols+31);     /* WINDOWS95 */
    else if (platform_code_1==28) 
      if_result_temp_1 = (Obj)(tl_versions_symbols+32);     /* FREEBSD */
    else if (platform_code_1==29) 
      if_result_temp_1 = (Obj)(tl_versions_symbols+33);     /* LINUX */
    else if (platform_code_1==30) 
      if_result_temp_1 = (Obj)(tl_versions_symbols+34);     /* MACOSX */
    else 
      if_result_temp_1 = (Obj)(tl_versions_symbols+35);     /* EXPERIMENTAL-PORT */
    g2_machine_type = if_result_temp_1;
  }
  if (machine_model_var==(Obj)(&Unbound)) 
    machine_model_var = (Obj)NULL;
  return;
}

