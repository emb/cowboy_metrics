PROJECT = cowboy_metrics

CT_OPTS = -cover test/cover.spec -config test/snmp.config
CT_SUITES = cm_www_mib cm_eunit cm_requests
TEST_DEPS = cowboy ibrowse
dep_cowboy = pkg://cowboy 0.9.0
dep_ibrowse = https://github.com/cmullaparthi/ibrowse.git v4.0.2


## Override the default target

default: mibs all

## Include standard targets

include erlang.mk

## Extensions to compile the mib files

MIB_FILES = \
	SYSAPPL-MIB.mib \
	WWW-MIB.mib     \
	NETWORK-SERVICES-MIB.mib  ## Used for applTCPProtoID

MIB_DIR = mibs
MIB_OUTPUT_DIR = priv/mibs
HRL_OUTPUT_DIR = include

ERLC_MIB_OPTS = \
	-I ./priv/mibs \
	+'{verbosity, info}'

TARGET_FILES = \
  $(MIB_FILES:%.mib=$(MIB_OUTPUT_DIR)/%.bin) \
  $(MIB_FILES:%.mib=$(HRL_OUTPUT_DIR)/%.hrl)


clean-mibs:
	$(gen_verbose) rm -f $(TARGET_FILES)


mibs: $(TARGET_FILES)


$(MIB_OUTPUT_DIR)/%.bin: $(MIB_DIR)/%.mib
	@mkdir -p $(MIB_OUTPUT_DIR)
	$(erlc_verbose) erlc -v $(ERLC_MIB_OPTS) -o $(MIB_OUTPUT_DIR) $<


$(HRL_OUTPUT_DIR)/%.hrl: $(MIB_OUTPUT_DIR)/%.bin
	@mkdir -p $(HRL_OUTPUT_DIR)
	$(erlc_verbose) erlc -v $(ERLC_MIB_OPTS) -o $(HRL_OUTPUT_DIR) $<
