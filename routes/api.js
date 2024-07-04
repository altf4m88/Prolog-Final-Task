const express = require("express");
const router = express.Router();
const { Favorite } = require("../models");

// Mendapatkan semua data favorit
router.get("/get_favorites", async (req, res) => {
  try {
    const favorites = await Favorite.findAll();
    res.json(favorites);
  } catch (error) {
    res.status(500).json({ message: "Error fetching favorites" });
  }
});

// Menambahkan favorit
router.post("/add_favorite", async (req, res) => {
  const { from, to } = req.body;
  try {
    const favorite = await Favorite.create({
      from_currency: from,
      to_currency: to,
    });
    res.json({ status: "success", favorite });
  } catch (error) {
    res.status(500).json({ message: "Error adding favorite" });
  }
});

module.exports = router;
