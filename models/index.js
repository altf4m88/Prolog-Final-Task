const { Sequelize, DataTypes } = require("sequelize");

const sequelize = new Sequelize("db_currency", "root", "", {
  host: "localhost",
  dialect: "mysql",
});

const Favorite = sequelize.define(
  "Favorite",
  {
    from_currency: {
      type: DataTypes.STRING,
      allowNull: false,
    },
    to_currency: {
      type: DataTypes.STRING,
      allowNull: false,
    },
  },
  {
    tableName: "tbl_favorites",
    timestamps: false,
  }
);

const LogRequest = sequelize.define(
  "LogRequest",
  {
    from_to_currency: {
      type: DataTypes.STRING,
      allowNull: false,
    },
    request_count: {
      type: DataTypes.INTEGER,
      allowNull: false,
    },
  },
  {
    tableName: "tbl_log_requests",
    timestamps: false,
  }
);

sequelize.sync();

module.exports = { sequelize, Favorite, LogRequest };
