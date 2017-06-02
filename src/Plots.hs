{-# LANGUAGE ScopedTypeVariables #-}
module Plots where


import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Control.Monad
import Database
import Control.Lens
import Data.Text (Text)
import qualified Data.Text as T
import Graphics.Rendering.Chart.Layout
import Data.Default
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (sortBy)





topLastWeek :: IO (Maybe FilePath)
topLastWeek = do
    last <- lastWeek
    all <- liftM (take 5 . reverse . sortBy (\(_, x) (_, y) -> compare x y)) . runDB $ messagesFromGroup last
    let users = map (T.unpack . fst) all
        values = map (\(_, v) -> [v]) all
    renderableToFile def "plot.png" (chart users values)
    return (Just "plot.png")
    
    where
        chart labels values = toRenderable (layout labels values)
        layout labels values = layout_title .~ "Last week's TOP 5"
                $ layout_x_axis . laxis_generate .~ (\i ->
                    let index = autoIndexAxis labels i 
                    in index {
                        _axis_visibility = AxisVisibility True False True
                    })
                $ layout_plots .~ [ plotBars (bars values)]
                $ def :: Layout PlotIndex Int

        bars values = plot_bars_titles .~ ["User"]
                $ plot_bars_style .~ BarsStacked
                $ plot_bars_values .~ addIndexes values
                $ plot_bars_spacing .~ BarsFixGap 30 5
                $ def

        miny,maxy :: Double
        (miny,maxy) = (-0.2,3.2)
