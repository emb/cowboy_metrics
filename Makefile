PROJECT = cowboy_metrics

CT_SUITES = request_counter
TEST_DEPS = cowboy ibrowse
dep_cowboy = pkg://cowboy 0.9.0
dep_ibrowse = https://github.com/cmullaparthi/ibrowse.git v4.0.2

## Override the default target

default: mibs all

## Include standard targets

include erlang.mk

## Extensions to compile the mib files

MIB_FILES = COWBOY-MIB.mib

MIB_DIR = mibs/
MIB_OUTPUT_DIR = priv/mibs
HRL_OUTPUT_DIR = include

ERLC_MIB_OPTS = +'{il, ["otp_mibs/priv/mibs/"]}' +'{verbosity, info}'

TARGET_FILES = \
  $(MIB_FILES:%.mib=$(MIB_OUTPUT_DIR)/%.bin) \
  $(MIB_FILES:%.mib=$(HRL_OUTPUT_DIR)/%.hrl)


clean-mibs:
	$(gen_verbose) rm -f $(TARGET_FILES)


mibs: $(TARGET_FILES)


$(MIB_OUTPUT_DIR)/%.bin:
	@mkdir -p $(MIB_OUTPUT_DIR)
	$(erlc_verbose) erlc -v $(ERLC_MIB_OPTS) -o $(MIB_OUTPUT_DIR) \
	$(addprefix $(MIB_DIR), $(MIB_FILES))

$(HRL_OUTPUT_DIR)/%.hrl: $(MIB_OUTPUT_DIR)/%.bin
	@mkdir -p $(HRL_OUTPUT_DIR)
	$(erlc_verbose) erlc -v $(ERLC_MIB_OPTS) -o $(HRL_OUTPUT_DIR) $<
