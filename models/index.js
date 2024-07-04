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

sequelize.sync();

module.exports = { sequelize, Favorite };
