{ config, lib, pkgs, ... }:

let
  cfg = config.services.nft-marketplace-server;
in
{

  options.services.nft-marketplace-server = {

    enable = lib.mkEnableOption "Enable Seabug backend";

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.nft-marketplace-server;
    };

    port = lib.mkOption {
      type = lib.types.int;
      default = 8008;
    };

    user = lib.mkOption {
      type = lib.types.str;
      default = "nft-marketplace-server";
    };

    group = lib.mkOption {
      type = lib.types.str;
      default = "nft-marketplace-server";
    };

    configurePostgresql = lib.mkOption {
      type = lib.types.bool;
      default = true;
    };

    db = lib.mkOption {
      type = lib.types.str;
      default = "nft-marketplace-server";
    };

    databaseConnection = lib.mkOption {
      type = lib.types.str;
      default = "host=/run/postgresql port=${toString config.services.postgresql.port} dbname=${cfg.db}";
    };

    dataDir = lib.mkOption {
      type = lib.types.str;
      default = "/var/lib/nft-marketplace-server";
    };

    # TODO never store secret keys like this, they would end in the Nix store
    nftStorageKey = lib.mkOption {
      type = lib.types.str;
      default = "";
    };

  };

  config = lib.mkIf cfg.enable {

    users.users.nft-marketplace-server = lib.mkIf (cfg.user == "nft-marketplace-server") {
      isSystemUser = true;
      group = cfg.group;
    };

    users.groups.nft-marketplace-server = lib.mkIf (cfg.group == "nft-marketplace-server") { };

    services.postgresql = lib.mkIf cfg.configurePostgresql {
      enable = true;
      ensureDatabases = [ cfg.db ];
      ensureUsers = [{
        name = cfg.user;
        ensurePermissions = {
          "DATABASE \"${cfg.db}\"" = "ALL PRIVILEGES";
        };
      }];
    };

    systemd.services.nft-marketplace-server = {
      enable = true;
      after = [ "postgresql.service" ];
      wantedBy = [ "multi-user.target" ];

      script = with lib; escapeShellArgs (concatLists [
        [ "${cfg.package}/bin/nft-marketplace-server" ]
        [ "--db-connection" cfg.databaseConnection ]
        [ "--nft-storage-key" cfg.nftStorageKey ]
        [ "--port" "${toString cfg.port}" ]
        [ "--image-folder" cfg.dataDir ]
      ]);

      serviceConfig = {
        User = cfg.user;
        Group = cfg.group;
        RuntimeDirectory = "nft-marketplace-server";
        RuntimeDirectoryMode = "0750";
        StateDirectory = "nft-marketplace-server";

        # Security
        UMask = "0077";
        AmbientCapabilities = [ "CAP_NET_BIND_SERVICE" ];
        CapabilityBoundingSet = [ "CAP_NET_BIND_SERVICE" ];
        ProcSubset = "pid";
        ProtectProc = "invisible";
        NoNewPrivileges = true;
        DevicePolicy = "closed";
        ProtectSystem = "strict";
        ProtectHome = true;
        PrivateTmp = true;
        PrivateDevices = true;
        PrivateUsers = true;
        ProtectHostname = true;
        ProtectClock = true;
        ProtectKernelTunables = true;
        ProtectKernelModules = true;
        ProtectKernelLogs = true;
        ProtectControlGroups = true;
        RestrictAddressFamilies = [ "AF_UNIX" "AF_INET" "AF_INET6" ];
        RestrictNamespaces = true;
        LockPersonality = true;
        RestrictRealtime = true;
        RestrictSUIDSGID = true;
        RemoveIPC = true;
        PrivateMounts = true;
        SystemCallArchitectures = "native";
        SystemCallFilter = [ "~@cpu-emulation @debug @keyring @mount @obsolete @privileged @setuid @resources" ];
      };
    };
  };
}
